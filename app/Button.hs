module Button where

import Clash.Prelude
import Types

-- === Button Processing ===

-- Button debouncing (using abstract types)
buttonDebounce :: (HiddenClockResetEnable dom) => Signal dom Bit -> Signal dom ButtonState
buttonDebounce btnRaw = debounced
  where
    debounceTime = SNat @1_350_000 -- 50ms at 27MHz
    counter = regEn (0 :: Unsigned 21) (pure True) nextCounter
    nextCounter =
      mux
        (btnRaw .==. pure high)
        (satAdd SatBound <$> counter <*> pure 1)
        (pure 0)
    debounced =
      mux
        (counter .>=. pure (snatToNum debounceTime))
        (pure Pressed)
        (pure Released)

-- Button event detection
buttonEventDetector :: (HiddenClockResetEnable dom) => Signal dom ButtonState -> Signal dom ButtonEvent
buttonEventDetector btnState = event
  where
    prev1 = register Released btnState
    prev2 = register Released prev1

    -- Detect stable state changes
    stablePress =
      liftA3
        (\curr p1 p2 -> curr == Pressed && p1 == Released && p2 == Released)
        btnState
        prev1
        prev2
    stableRelease =
      liftA3
        (\curr p1 p2 -> curr == Released && p1 == Pressed && p2 == Pressed)
        btnState
        prev1
        prev2

    event =
      mux
        stablePress
        (pure ButtonPress)
        ( mux
            stableRelease
            (pure ButtonRelease)
            (pure NoEvent)
        )

-- Button processing pipeline
processButtons ::
  (HiddenClockResetEnable dom) =>
  Signal dom Bit -> -- Button 1 raw signal
  Signal dom Bit -> -- Button 2 raw signal
  (Signal dom ButtonEvent, Signal dom ButtonEvent) -- (Button 1 event, Button 2 event)
processButtons btn1Raw btn2Raw = (btn1Event, btn2Event)
  where
    btn1State = buttonDebounce btn1Raw
    btn2State = buttonDebounce btn2Raw
    btn1Event = buttonEventDetector btn1State
    btn2Event = buttonEventDetector btn2State