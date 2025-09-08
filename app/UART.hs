module UART where

import Clash.Prelude
import Types

-- === UART Configuration ===
-- For 27MHz clock, 115200 baud
-- Divider = 27,000,000 / 115200 ≈ 234
baudDivider :: Unsigned 8
baudDivider = 234

-- === UART Transmitter ===

-- ASCII conversion utility
digitToAscii :: Unsigned 4 -> Unsigned 8
digitToAscii d = 0x30 + resize d

-- Convert LED position to digit
ledPositionToDigit :: LedPosition -> Unsigned 4
ledPositionToDigit Led0 = 0
ledPositionToDigit Led1 = 1
ledPositionToDigit Led2 = 2
ledPositionToDigit Led3 = 3
ledPositionToDigit Led4 = 4
ledPositionToDigit Led5 = 5

-- Simple UART transmitter
uartTx ::
  (HiddenClockResetEnable dom) =>
  Signal dom Bool -> -- Start transmission
  Signal dom (Unsigned 8) -> -- Transmit data
  (Signal dom Bit, Signal dom Bool) -- (TX line, Busy)
uartTx start txData = (txLine, busy)
  where
    -- Baud rate counter
    baudCnt =
      register (0 :: Unsigned 8)
        $ mux (register False start) (pure 0)
        $ mux (baudCnt .==. pure (baudDivider - 1)) (pure 0)
        $ (pure 1 + baudCnt)

    baudTick = baudCnt .==. pure (baudDivider - 1)

    -- TX state machine
    txState =
      register TxIdle
        $ mux
          (txState .==. pure TxIdle)
          (mux start (pure TxStart) (pure TxIdle))
        $ mux
          (txState .==. pure TxStart)
          (mux baudTick (pure TxData) (pure TxStart))
        $ mux
          (txState .==. pure TxData)
          (mux (baudTick .&&. (bitCnt .==. pure 8)) (pure TxStop) (pure TxData))
        $ mux baudTick (pure TxIdle) (pure TxStop)

    -- Bit counter
    bitCnt =
      register (0 :: Unsigned 4)
        $ mux (txState .==. pure TxIdle) (pure 0)
        $ mux (txState .==. pure TxData .&&. baudTick) (pure 1 + bitCnt)
        $ bitCnt

    -- Shift register (10 bits: start + 8 data + stop)
    shiftData =
      register (0x3FF :: Unsigned 10)
        $
        -- start 立ち上がりで [stop=1][data<<1][start=0] をロード
        mux
          start
          ((\d -> (0x200 :: Unsigned 10) .|. (resize d `shiftL` 1)) <$> txData)
          -- 送信中はボーレート毎に右シフト、MSB に 1 を詰める
          ( mux
              ( ( txState .==. pure TxStart
                    .||. txState .==. pure TxData
                    .||. txState .==. pure TxStop
                )
                  .&&. baudTick
              )
              ((.|. 0x200) . (`shiftR` 1) <$> shiftData)
              shiftData
          )
    -- Output
    txLine = lsb <$> shiftData
    busy = txState ./=. pure TxIdle

data RxS = RxS
  { st :: UartRxState,
    osc :: Unsigned 4,
    bc :: Unsigned 4,
    sh :: Unsigned 8
  }
  deriving (Generic, NFDataX)

-- === UART Receiver (NCO + 16x oversampling; start→half→8×full→stop) ===
uartRx ::
  (HiddenClockResetEnable dom) =>
  Signal dom Bit -> -- RX line (idle=high)
  (Signal dom (Unsigned 8), Signal dom Bool) -- (data, valid 1clk)
uartRx rxLine = (rxData, dataValid)
  where
    -- 2段同期
    rxSync = register high (register high rxLine)

    -- NCO: acc += inc; if acc >= modv then acc -= modv; osTick := accTemp >= modv
    clockHz :: Unsigned 48
    clockHz = 27000000
    baudRate :: Unsigned 48
    baudRate = 115200
    osRate :: Unsigned 8
    osRate = 16

    incVal :: Unsigned 48
    incVal = baudRate * resize osRate
    modvVal :: Unsigned 48
    modvVal = clockHz

    acc = register 0 accNext
    accTemp = (+) <$> acc <*> pure incVal
    osTick = (>=) <$> accTemp <*> pure modvVal
    accNext = mux osTick ((-) <$> accTemp <*> pure modvVal) accTemp

    -- mealy 状態

    s0 = RxS {st = RxIdle, osc = 0, bc = 0, sh = 0}

    osMax :: Unsigned 4
    osMax = 15 -- OS-1
    halfM1 :: Unsigned 4
    halfM1 = 8

    -- 1tick ごとに状態を進める。tick=0 の間は保持。
    step :: RxS -> (Bool, Bit) -> (RxS, (Unsigned 8, Bool))
    step s (tick, rx) =
      if not tick
        then (s, (sh s, False))
        else case st s of
          RxIdle ->
            if rx == low
              then (s {st = RxStart, osc = 0, bc = 0}, (sh s, False))
              else (s {osc = 0, bc = 0}, (sh s, False))
          -- スタート中央で再確認（ノイズ除去）
          RxStart ->
            if osc s == halfM1
              then
                if rx == low
                  then (s {st = RxData, osc = 0, bc = 0}, (sh s, False))
                  else (s {st = RxIdle, osc = 0, bc = 0}, (sh s, False))
              else (s {osc = osc s + 1}, (sh s, False))
          -- データ：各ビットの最後でサンプル（中央相当）
          RxData ->
            if osc s == osMax
              then
                let sh' = ((if rx == high then 0x80 else 0x00) :: Unsigned 8) .|. (sh s `shiftR` 1)
                    bc' = bc s + 1
                    st' = if bc s == 7 then RxStop else RxData
                 in (s {st = st', osc = 0, bc = bc', sh = sh'}, (sh', False))
              else (s {osc = osc s + 1}, (sh s, False))
          -- ストップ中央で valid を 1clk パルス
          RxStop ->
            if osc s == osMax
              then (s {st = RxIdle, osc = 0, bc = 0}, (sh s, rx == high))
              else (s {osc = osc s + 1}, (sh s, False))

    (rxData, dataValid) =
      unbundle (mealy step s0 (bundle (osTick, rxSync)))

-- === Message Transmission Control ===

-- Count message transmitter (sends "CNT: N\r\n")
countMessageTx ::
  (HiddenClockResetEnable dom) =>
  Signal dom LedPosition -> -- Current LED position
  Signal dom Bool -> -- Position change trigger
  (Signal dom Bit, Signal dom Bool) -- (TX line, Busy)
countMessageTx ledPos trigger = (txOut, busy)
  where
    -- Message bytes for each position
    msgBytes :: LedPosition -> Vec 8 (Unsigned 8)
    msgBytes pos =
      0x43
        :> 0x4E
        :> 0x54
        :> 0x3A
        :> 0x20
        :> digitToAscii (ledPositionToDigit pos)
        :> 0x0D
        :> 0x0A
        :> Nil

    -- Message transmission state
    sendState =
      register False
        $ mux trigger (pure True)
        $ mux (sendState .&&. not <$> txBusy .&&. (msgIndex .==. pure 7)) (pure False)
        $ sendState

    -- Message byte index
    msgIndex =
      register (0 :: Unsigned 3)
        $ mux trigger (pure 0)
        $ mux (sendState .&&. not <$> txBusy .&&. (msgIndex .<. pure 7)) (pure 1 + msgIndex)
        $ msgIndex

    -- Current message
    currentMsg = regEn (msgBytes Led0) trigger (msgBytes <$> ledPos)

    -- Start transmission
    startTx = trigger .||. (sendState .&&. not <$> txBusy .&&. (msgIndex .<. pure 7))

    -- Current byte to send
    currentByte = liftA2 (!!) currentMsg msgIndex

    -- UART transmitter
    (txOut, txBusy) = uartTx startTx currentByte
    busy = sendState

-- === Key Input Processing ===

-- Convert received keys to button events
keyToButtonEvents ::
  (HiddenClockResetEnable dom) =>
  Signal dom (Unsigned 8) -> -- Received data
  Signal dom Bool -> -- Data valid
  (Signal dom ButtonEvent, Signal dom ButtonEvent) -- (btn1Event, btn2Event)
keyToButtonEvents rxData dataValid = (btn1Event, btn2Event)
  where
    -- Key mappings
    isBtn1Key =
      dataValid
        .&&. ( (rxData .==. pure 0x31)
                 .||. (rxData .==. pure 0x41) -- '1'
                 .||. (rxData .==. pure 0x61) -- 'A'
                 -- 'a'
             )

    isBtn2Key =
      dataValid
        .&&. ( (rxData .==. pure 0x32)
                 .||. (rxData .==. pure 0x20) -- '2'
                 .||. (rxData .==. pure 0x0D) -- Space
                 -- Enter
             )

    -- Generate button events
    btn1Event = mux isBtn1Key (pure ButtonPress) (pure NoEvent)
    btn2Event = mux isBtn2Key (pure ButtonPress) (pure NoEvent)