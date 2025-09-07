module Types where

import Clash.Prelude

-- === Custom Type Definitions ===

-- Operation mode
data Mode = Auto | Manual
  deriving (Generic, NFDataX, Eq, Show)

-- LED position
data LedPosition = Led0 | Led1 | Led2 | Led3 | Led4 | Led5
  deriving (Generic, NFDataX, Eq, Show, Enum, Bounded)

-- Button state
data ButtonState = Pressed | Released
  deriving (Generic, NFDataX, Eq, Show)

-- Button event
data ButtonEvent = ButtonPress | ButtonRelease | NoEvent
  deriving (Generic, NFDataX, Eq, Show)

-- LED control command
data LedCommand = Advance | Hold
  deriving (Generic, NFDataX, Eq, Show)

-- UART transmit state
data UartTxState = TxIdle | TxStart | TxData | TxStop
  deriving (Generic, NFDataX, Eq, Show)

-- UART receive state
data UartRxState = RxIdle | RxStart | RxData | RxStop
  deriving (Generic, NFDataX, Eq, Show)

-- === Utility Functions ===

-- Toggle mode
toggleMode :: Mode -> Mode
toggleMode Auto = Manual
toggleMode Manual = Auto

-- Move to next LED position
nextLedPosition :: LedPosition -> LedPosition
nextLedPosition Led5 = Led0
nextLedPosition pos = succ pos

-- Convert LED position to bit pattern
ledPositionToBits :: LedPosition -> Vec 6 Bit
ledPositionToBits pos = invertBits $ vecToBits $ vecFromIndex (fromEnum pos)
  where
    vecFromIndex :: Int -> Vec 6 Bool
    vecFromIndex idx = replace (fromIntegral idx) True (repeat False)

    vecToBits :: Vec 6 Bool -> Vec 6 Bit
    vecToBits = map boolToBit

    invertBits :: Vec 6 Bit -> Vec 6 Bit
    invertBits = map complement