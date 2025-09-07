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
        $ mux start (liftA2 (\d -> \_ -> 0x200 .|. resize d) txData (pure ()))
        $ mux
          ((txState .==. pure TxStart .||. txState .==. pure TxData .||. txState .==. pure TxStop) .&&. baudTick)
          ((`shiftR` 1) <$> shiftData)
        $ shiftData

    -- Output
    txLine = lsb <$> shiftData
    busy = txState ./=. pure TxIdle

-- === UART Receiver ===

uartRx ::
  (HiddenClockResetEnable dom) =>
  Signal dom Bit -> -- RX line
  (Signal dom (Unsigned 8), Signal dom Bool) -- (Received data, Data valid)
uartRx rxLine = (rxData, dataValid)
  where
    -- Synchronize RX input
    rxSync = register high $ register high rxLine

    -- Baud rate counter (sample at middle)
    baudCnt =
      register (0 :: Unsigned 8)
        $ mux (rxState .==. pure RxIdle) (pure 0)
        $ mux (baudCnt .==. pure (baudDivider `div` 2 - 1)) (pure 0)
        $ (pure 1 + baudCnt)

    baudTick = baudCnt .==. pure (baudDivider `div` 2 - 1)

    -- RX state machine
    rxState =
      register RxIdle
        $ mux
          (rxState .==. pure RxIdle)
          (mux (rxSync .==. pure low) (pure RxStart) (pure RxIdle))
        $ mux
          (rxState .==. pure RxStart)
          (mux baudTick (pure RxData) (pure RxStart))
        $ mux
          (rxState .==. pure RxData)
          (mux (baudTick .&&. (bitCnt .==. pure 7)) (pure RxStop) (pure RxData))
        $ mux baudTick (pure RxIdle) (pure RxStop)

    -- Bit counter
    bitCnt =
      register (0 :: Unsigned 4)
        $ mux (rxState .==. pure RxIdle) (pure 0)
        $ mux (rxState .==. pure RxData .&&. baudTick) (pure 1 + bitCnt)
        $ bitCnt

    -- Shift register for received data
    shiftData =
      register (0 :: Unsigned 8)
        $ mux
          (rxState .==. pure RxData .&&. baudTick)
          (liftA2 (\sr rx -> (sr `shiftR` 1) .|. if rx == high then 0x80 else 0x00) shiftData rxSync)
        $ shiftData

    -- Data valid flag
    validFlag =
      register False
        $ mux (rxState .==. pure RxStop .&&. baudTick) (pure True)
        $ (pure False)

    -- Output
    rxData = shiftData
    dataValid = validFlag

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