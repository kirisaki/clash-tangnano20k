module UART
  ( uartTx,
    uartRx,
    countMessageTx,
    keyToButtonEvents,
  )
where

import Clash.Prelude
import Types

-- =============================================================================
-- UART Transmitter
-- =============================================================================

-- | UART transmitter with 8N1 format (8 data bits, no parity, 1 stop bit)
-- Transmits at 115200 baud with 27MHz clock
uartTx ::
  (HiddenClockResetEnable dom) =>
  -- | Start transmission trigger
  Signal dom Bool ->
  -- | Data byte to transmit
  Signal dom (Unsigned 8) ->
  -- | (TX line output, Busy flag)
  (Signal dom Bit, Signal dom Bool)
uartTx start txData = (txLine, busy)
  where
    -- Baud rate timing counter
    baudCnt = register (0 :: Unsigned 8) baudCntNext
    baudCntNext =
      mux
        (register False start)
        (pure 0)
        ( mux
            (baudCnt .==. pure (uartBaudDivider - 1))
            (pure 0)
            (baudCnt + pure 1)
        )

    baudTick = baudCnt .==. pure (uartBaudDivider - 1)

    -- Transmitter state machine
    txState = register TxIdle txStateNext
    txStateNext =
      mux
        (txState .==. pure TxIdle)
        (mux start (pure TxStart) (pure TxIdle))
        ( mux
            (txState .==. pure TxStart)
            (mux baudTick (pure TxData) (pure TxStart))
            ( mux
                (txState .==. pure TxData)
                ( mux
                    (baudTick .&&. (bitCnt .==. pure 8))
                    (pure TxStop)
                    (pure TxData)
                )
                (mux baudTick (pure TxIdle) (pure TxStop))
            )
        )

    -- Bit counter for data transmission
    bitCnt = register (0 :: Unsigned 4) bitCntNext
    bitCntNext =
      mux
        (txState .==. pure TxIdle)
        (pure 0)
        ( mux
            (txState .==. pure TxData .&&. baudTick)
            (bitCnt + pure 1)
            bitCnt
        )

    -- 10-bit shift register: [stop][data(7:0)][start]
    shiftData = register (0x3FF :: Unsigned 10) shiftDataNext
    shiftDataNext =
      mux
        start
        ((\d -> (0x200 :: Unsigned 10) .|. (resize d `shiftL` 1)) <$> txData)
        ( mux
            (isTxActive .&&. baudTick)
            ((.|. 0x200) . (`shiftR` 1) <$> shiftData)
            shiftData
        )

    isTxActive =
      (txState .==. pure TxStart)
        .||. (txState .==. pure TxData)
        .||. (txState .==. pure TxStop)

    -- Output signals
    txLine = lsb <$> shiftData
    busy = txState ./=. pure TxIdle

-- =============================================================================
-- UART Receiver
-- =============================================================================

-- | UART receiver with 16x oversampling and NCO-based baud rate generation
-- Receives 8N1 format at 115200 baud
uartRx ::
  (HiddenClockResetEnable dom) =>
  -- | RX line input (idle = high)
  Signal dom Bit ->
  -- | (Received data, Valid pulse)
  (Signal dom (Unsigned 8), Signal dom Bool)
uartRx rxLine = (rxData, dataValid)
  where
    -- Two-stage synchronizer for RX input
    rxSync = register high (register high rxLine)

    -- NCO (Numerically Controlled Oscillator) for oversampling clock
    -- Generates 16x oversampling rate from system clock
    clockHz = 27_000_000 :: Unsigned 48
    baudRate = 115_200 :: Unsigned 48
    osRate = 16 :: Unsigned 48

    incVal = baudRate * osRate
    modVal = clockHz

    ncoAcc = register 0 ncoAccNext
    ncoTemp = ncoAcc + pure incVal
    osTick = ncoTemp .>=. pure modVal
    ncoAccNext = mux osTick (ncoTemp - pure modVal) ncoTemp

    -- Initial state for receiver state machine
    rxInitState =
      UartRxInternalState
        { rxState = RxIdle,
          oversampleCnt = 0,
          bitCnt = 0,
          shiftReg = 0
        }

    -- Receiver state machine (Mealy machine)
    (rxData, dataValid) =
      unbundle
        $ mealy rxStateMachine rxInitState (bundle (osTick, rxSync))

-- | UART receiver state machine implementation
rxStateMachine ::
  UartRxInternalState ->
  (Bool, Bit) ->
  (UartRxInternalState, (Unsigned 8, Bool))
rxStateMachine s (tick, rx)
  | not tick = (s, (shiftReg s, False)) -- Hold state when no tick
  | otherwise = case rxState s of
      RxIdle ->
        if rx == low
          then (s {rxState = RxStart, oversampleCnt = 0, bitCnt = 0}, (shiftReg s, False))
          else (s {oversampleCnt = 0, bitCnt = 0}, (shiftReg s, False))
      RxStart ->
        if oversampleCnt s == 8 -- Half bit time (center of start bit)
          then
            if rx == low
              then (s {rxState = RxData, oversampleCnt = 0, bitCnt = 0}, (shiftReg s, False))
              else (s {rxState = RxIdle, oversampleCnt = 0, bitCnt = 0}, (shiftReg s, False))
          else (s {oversampleCnt = oversampleCnt s + 1}, (shiftReg s, False))
      RxData ->
        if oversampleCnt s == 15 -- End of bit time
          then
            let newShift = (if rx == high then 0x80 else 0x00) .|. (shiftReg s `shiftR` 1)
                newBitCnt = bitCnt s + 1
                newState = if bitCnt s == 7 then RxStop else RxData
             in ( s {rxState = newState, oversampleCnt = 0, bitCnt = newBitCnt, shiftReg = newShift},
                  (newShift, False)
                )
          else (s {oversampleCnt = oversampleCnt s + 1}, (shiftReg s, False))
      RxStop ->
        if oversampleCnt s == 15 -- End of stop bit
          then
            ( s {rxState = RxIdle, oversampleCnt = 0, bitCnt = 0},
              (shiftReg s, rx == high) -- Valid only if stop bit is high
            )
          else (s {oversampleCnt = oversampleCnt s + 1}, (shiftReg s, False))

-- =============================================================================
-- Message Transmission
-- =============================================================================

-- | Transmit count messages in format "CNT: N\r\n" when LED position changes
countMessageTx ::
  (HiddenClockResetEnable dom) =>
  -- | Current LED position
  Signal dom LedPosition ->
  -- | Position change trigger
  Signal dom Bool ->
  -- | (TX line, Busy flag)
  (Signal dom Bit, Signal dom Bool)
countMessageTx ledPos trigger = (txOut, busy)
  where
    -- Pre-computed message bytes for each LED position
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
    -- "CNT: N\r\n" where N is the LED position digit

    -- Message transmission state management
    sendState = register False sendStateNext
    sendStateNext =
      mux
        trigger
        (pure True)
        ( mux
            (sendState .&&. not <$> txBusy .&&. (msgIndex .==. pure 7))
            (pure False)
            sendState
        )

    -- Message byte index counter
    msgIndex = register (0 :: Unsigned 3) msgIndexNext
    msgIndexNext =
      mux
        trigger
        (pure 0)
        ( mux
            (sendState .&&. not <$> txBusy .&&. (msgIndex .<. pure 7))
            (msgIndex + pure 1)
            msgIndex
        )

    -- Current message buffer
    currentMsg = regEn (msgBytes Led0) trigger (msgBytes <$> ledPos)

    -- Transmission control
    startTx = trigger .||. (sendState .&&. not <$> txBusy .&&. (msgIndex .<. pure 7))
    currentByte = liftA2 (!!) currentMsg msgIndex

    -- UART transmitter instance
    (txOut, txBusy) = uartTx startTx currentByte
    busy = sendState

-- =============================================================================
-- Keyboard Input Processing
-- =============================================================================

-- | Convert received UART characters to button events
-- Supports multiple key mappings for each button:
-- Button 1: '1', 'A', 'a' (mode toggle)
-- Button 2: '2', ' ' (space), '\r' (enter) (manual advance)
keyToButtonEvents ::
  (HiddenClockResetEnable dom) =>
  -- | Received character
  Signal dom (Unsigned 8) ->
  -- | Data valid flag
  Signal dom Bool ->
  -- | (Button 1 event, Button 2 event)
  (Signal dom ButtonEvent, Signal dom ButtonEvent)
keyToButtonEvents rxData dataValid = (btn1Event, btn2Event)
  where
    -- Button 1 key detection (mode toggle)
    isBtn1Key =
      dataValid
        .&&. ( (rxData .==. pure 0x31)
                 .||. (rxData .==. pure 0x41) -- '1'
                 .||. (rxData .==. pure 0x61) -- 'A'
                 -- 'a'
             )

    -- Button 2 key detection (advance)
    isBtn2Key =
      dataValid
        .&&. ( (rxData .==. pure 0x32)
                 .||. (rxData .==. pure 0x20) -- '2'
                 .||. (rxData .==. pure 0x0D) -- ' ' (space)
                 -- '\r' (enter)
             )

    -- Generate button events
    btn1Event = mux isBtn1Key (pure ButtonPress) (pure NoEvent)
    btn2Event = mux isBtn2Key (pure ButtonPress) (pure NoEvent)