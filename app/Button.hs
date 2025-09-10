module Button
  ( buttonDebounce,
    buttonEventDetector,
    processButtons,
  )
where

import Clash.Prelude
import Types

-- =============================================================================
-- Button Debouncing
-- =============================================================================

-- | Debounce a raw button signal to produce stable button state
-- Uses a counter that must reach the debounce threshold while button is pressed
-- to register as a stable press state
buttonDebounce ::
  (HiddenClockResetEnable dom) =>
  Signal dom Bit ->
  Signal dom ButtonState
buttonDebounce btnRaw = debounced
  where
    debounceThreshold = SNat @1_350_000 -- 50ms at 27MHz

    -- Counter increments while button is pressed, resets when released
    counter = regEn (0 :: Unsigned 21) (pure True) nextCounter
    nextCounter =
      mux
        (btnRaw .==. pure high)
        (satAdd SatBound <$> counter <*> pure 1)
        (pure 0)

    -- Output stable state when counter reaches threshold
    debounced =
      mux
        (counter .>=. pure (snatToNum debounceThreshold))
        (pure Pressed)
        (pure Released)

-- =============================================================================
-- Button Event Detection
-- =============================================================================

-- | Detect button press/release events from debounced button state
-- Requires two consecutive stable samples to confirm state change
buttonEventDetector ::
  (HiddenClockResetEnable dom) =>
  Signal dom ButtonState ->
  Signal dom ButtonEvent
buttonEventDetector btnState = event
  where
    -- Two-stage delay for stable state change detection
    prev1 = register Released btnState
    prev2 = register Released prev1

    -- Detect stable press: current=Pressed, prev1=Released, prev2=Released
    stablePress =
      liftA3
        (\curr p1 p2 -> curr == Pressed && p1 == Released && p2 == Released)
        btnState
        prev1
        prev2

    -- Detect stable release: current=Released, prev1=Pressed, prev2=Pressed
    stableRelease =
      liftA3
        (\curr p1 p2 -> curr == Released && p1 == Pressed && p2 == Pressed)
        btnState
        prev1
        prev2

    -- Generate events based on stable state changes
    event =
      mux
        stablePress
        (pure ButtonPress)
        ( mux
            stableRelease
            (pure ButtonRelease)
            (pure NoEvent)
        )

-- =============================================================================
-- Complete Button Processing Pipeline
-- =============================================================================

-- | Complete button processing pipeline for two buttons
-- Takes raw button signals and produces debounced button events
processButtons ::
  (HiddenClockResetEnable dom) =>
  -- | Button 1 raw signal (active high)
  Signal dom Bit ->
  -- | Button 2 raw signal (active high)
  Signal dom Bit ->
  -- | (Button 1 events, Button 2 events)
  (Signal dom ButtonEvent, Signal dom ButtonEvent)
processButtons btn1Raw btn2Raw = (btn1Event, btn2Event)
  where
    -- Debounce both buttons
    btn1State = buttonDebounce btn1Raw
    btn2State = buttonDebounce btn2Raw

    -- Detect events from debounced states
    btn1Event = buttonEventDetector btn1State
    btn2Event = buttonEventDetector btn2State