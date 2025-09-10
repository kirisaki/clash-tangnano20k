module Types where

import Clash.Prelude

-- =============================================================================
-- Core System Types
-- =============================================================================

-- | Operation mode for LED controller
data Mode = Auto | Manual
  deriving (Generic, NFDataX, Eq, Show)

-- | LED position (6 LEDs total, 0-5)
data LedPosition = Led0 | Led1 | Led2 | Led3 | Led4 | Led5
  deriving (Generic, NFDataX, Eq, Show, Enum, Bounded)

-- | LED control command
data LedCommand = Advance | Hold
  deriving (Generic, NFDataX, Eq, Show)

-- =============================================================================
-- Button Types
-- =============================================================================

-- | Physical button state after debouncing
data ButtonState = Pressed | Released
  deriving (Generic, NFDataX, Eq, Show)

-- | Button event detection result
data ButtonEvent = ButtonPress | ButtonRelease | NoEvent
  deriving (Generic, NFDataX, Eq, Show)

-- =============================================================================
-- UART Types
-- =============================================================================

-- | UART transmitter state machine
data UartTxState = TxIdle | TxStart | TxData | TxStop
  deriving (Generic, NFDataX, Eq, Show)

-- | UART receiver state machine
data UartRxState = RxIdle | RxStart | RxData | RxStop
  deriving (Generic, NFDataX, Eq, Show)

-- | UART receiver internal state (for Mealy machine)
data UartRxInternalState = UartRxInternalState
  { rxState :: UartRxState,
    oversampleCnt :: Unsigned 4,
    bitCnt :: Unsigned 4,
    shiftReg :: Unsigned 8
  }
  deriving (Generic, NFDataX)

-- =============================================================================
-- Type Conversion and Utility Functions
-- =============================================================================

-- | Toggle between Auto and Manual modes
toggleMode :: Mode -> Mode
toggleMode Auto = Manual
toggleMode Manual = Auto

-- | Advance to next LED position (wraps around)
nextLedPosition :: LedPosition -> LedPosition
nextLedPosition Led5 = Led0
nextLedPosition pos = succ pos

-- | Convert LED position to 6-bit output vector (active low)
ledPositionToBits :: LedPosition -> Vec 6 Bit
ledPositionToBits pos = invertBits $ vecToBits $ vecFromIndex (fromEnum pos)
  where
    vecFromIndex :: Int -> Vec 6 Bool
    vecFromIndex idx = replace (fromIntegral idx) True (repeat False)

    vecToBits :: Vec 6 Bool -> Vec 6 Bit
    vecToBits = map boolToBit

    invertBits :: Vec 6 Bit -> Vec 6 Bit
    invertBits = map complement

-- | Convert LED position to single digit (0-5)
ledPositionToDigit :: LedPosition -> Unsigned 4
ledPositionToDigit Led0 = 0
ledPositionToDigit Led1 = 1
ledPositionToDigit Led2 = 2
ledPositionToDigit Led3 = 3
ledPositionToDigit Led4 = 4
ledPositionToDigit Led5 = 5

-- | Convert single digit to ASCII code
digitToAscii :: Unsigned 4 -> Unsigned 8
digitToAscii d = 0x30 + resize d

-- =============================================================================
-- Button Event Utilities
-- =============================================================================

-- | Combine two button events (OR operation - prioritizes button presses)
orButtonEvent :: ButtonEvent -> ButtonEvent -> ButtonEvent
orButtonEvent ButtonPress _ = ButtonPress
orButtonEvent _ ButtonPress = ButtonPress
orButtonEvent _ _ = NoEvent

-- | Signal-level button event combination
orButtonEventS ::
  Signal dom ButtonEvent ->
  Signal dom ButtonEvent ->
  Signal dom ButtonEvent
orButtonEventS = liftA2 orButtonEvent

-- =============================================================================
-- Hardware Constants
-- =============================================================================

-- | System clock frequency (27 MHz for Tang Nano 20K)
systemClockHz :: Unsigned 32
systemClockHz = 27_000_000

-- | UART baud rate (115200 bps)
uartBaudRate :: Unsigned 32
uartBaudRate = 115_200

-- | UART baud rate divider for 27MHz clock
-- Divider = 27,000,000 / 115,200 ≈ 234
uartBaudDivider :: Unsigned 8
uartBaudDivider = 234

-- | Button debounce time (50ms at 27MHz)
-- 50ms * 27MHz = 1,350,000 cycles
buttonDebounceTime :: Unsigned 21
buttonDebounceTime = 1_350_000

-- | LED advance interval (1 second at 27MHz)
ledAdvanceInterval :: Unsigned 32
ledAdvanceInterval = systemClockHz

-- | UART oversampling rate (16x)
uartOversampleRate :: Unsigned 8
uartOversampleRate = 16