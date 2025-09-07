module LED where

import Clash.Prelude
import Types

-- === LED Control Logic ===

-- LED controller
ledController ::
  (HiddenClockResetEnable dom) =>
  Signal dom ButtonEvent -> -- Button 1 event
  Signal dom ButtonEvent -> -- Button 2 event
  (Signal dom (Vec 6 Bit), Signal dom LedPosition, Signal dom Bool) -- (LED output, Current position, Position changed)
ledController btn1Event btn2Event = (ledOutput, ledPosition, positionChanged)
  where
    -- Mode management
    modeChangeEvent = btn1Event .==. pure ButtonPress
    mode = regEn Auto modeChangeEvent (toggleMode <$> mode)

    -- LED control command generation
    tick = riseEvery (SNat @27_000_000) -- 1 second interval
    manualAdvance = btn2Event .==. pure ButtonPress

    ledCommand =
      mux
        (mode .==. pure Auto)
        (mux tick (pure Advance) (pure Hold)) -- Auto mode
        (mux manualAdvance (pure Advance) (pure Hold)) -- Manual mode

    -- LED position management
    ledAdvanceEvent = ledCommand .==. pure Advance
    ledPosition = regEn Led0 ledAdvanceEvent (nextLedPosition <$> ledPosition)
    positionChanged = ledAdvanceEvent

    -- LED output generation
    ledOutput = ledPositionToBits <$> ledPosition