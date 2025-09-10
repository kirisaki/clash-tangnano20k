module LED
  ( ledController,
  )
where

import Clash.Prelude
import Types

-- =============================================================================
-- LED Control Logic
-- =============================================================================

-- | Main LED controller that manages mode switching and LED position advancement
-- Button 1: Toggle between Auto and Manual modes
-- Button 2: Advance LED position (Manual mode) or ignored (Auto mode)
-- Auto mode: LED advances automatically every second
-- Manual mode: LED advances only when Button 2 is pressed
ledController ::
  (HiddenClockResetEnable dom) =>
  -- | Button 1 event (mode toggle)
  Signal dom ButtonEvent ->
  -- | Button 2 event (manual advance)
  Signal dom ButtonEvent ->
  ( Signal dom (Vec 6 Bit),
    -- \^ LED output vector
    Signal dom LedPosition,
    -- \^ Current LED position
    Signal dom Bool
  )
-- \^ Position changed flag

ledController btn1Event btn2Event = (ledOutput, ledPosition, positionChanged)
  where
    -- Mode management: toggle on Button 1 press
    modeChangeEvent = btn1Event .==. pure ButtonPress
    mode = regEn Auto modeChangeEvent (toggleMode <$> mode)

    -- Generate LED control commands based on current mode
    ledCommand = generateLedCommand mode btn2Event

    -- LED position management: advance on command
    ledAdvanceEvent = ledCommand .==. pure Advance
    ledPosition = regEn Led0 ledAdvanceEvent (nextLedPosition <$> ledPosition)
    positionChanged = ledAdvanceEvent

    -- Convert LED position to output bit pattern
    ledOutput = ledPositionToBits <$> ledPosition

-- =============================================================================
-- LED Command Generation
-- =============================================================================

-- | Generate LED control commands based on current mode and inputs
generateLedCommand ::
  (HiddenClockResetEnable dom) =>
  Signal dom Mode ->
  Signal dom ButtonEvent ->
  Signal dom LedCommand
generateLedCommand mode btn2Event = ledCommand
  where
    -- Auto mode: advance every second (1Hz tick)
    autoTick = riseEvery (SNat @27_000_000) -- 1 second at 27MHz

    -- Manual mode: advance on Button 2 press
    manualAdvance = btn2Event .==. pure ButtonPress

    -- Select command based on mode
    ledCommand =
      mux
        (mode .==. pure Auto)
        (mux autoTick (pure Advance) (pure Hold)) -- Auto mode
        (mux manualAdvance (pure Advance) (pure Hold)) -- Manual mode