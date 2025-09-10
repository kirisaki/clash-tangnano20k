module Main where

import Button
import Clash.Prelude
import Control.Monad (when)
import qualified Data.Bits as B
import Data.Word
import LED
import Types
import UART
import qualified Prelude as P

-- =============================================================================
-- Main System Integration
-- =============================================================================

-- | Integrated LED controller system with button and UART input
-- Features:
-- - Button 1: Toggle between Auto and Manual LED modes
-- - Button 2: Manual LED advance (Manual mode only)
-- - UART: Keyboard input simulation for buttons
-- - LED: 6-LED position display with automatic advancement (Auto mode)
-- - UART TX: Position change notifications ("CNT: N\r\n")
mainSystem ::
  (HiddenClockResetEnable dom) =>
  -- | Button 1 raw input (active high, mode toggle)
  Signal dom Bit ->
  -- | Button 2 raw input (active high, advance)
  Signal dom Bit ->
  -- | UART RX input
  Signal dom Bit ->
  ( Signal dom (Vec 6 Bit),
    -- \^ LED output (active low)
    Signal dom Bit
  )
-- \^ UART TX output

mainSystem btn1Raw btn2Raw rxLine = (ledOutput, txLine)
  where
    -- Physical button processing with debouncing and event detection
    (btn1Event, btn2Event) = processButtons btn1Raw btn2Raw

    -- UART receiver for keyboard input simulation
    (rxData, rxValid) = uartRx rxLine

    -- Convert UART input to button events
    (uartBtn1Event, uartBtn2Event) = keyToButtonEvents rxData rxValid

    -- Combine physical button and UART button events
    combinedBtn1Event = orButtonEventS btn1Event uartBtn1Event
    combinedBtn2Event = orButtonEventS btn2Event uartBtn2Event

    -- LED controller with mode management and position control
    (ledOutput, currentPosition, positionChanged) =
      ledController combinedBtn1Event combinedBtn2Event

    -- UART transmitter for position change notifications
    (txLine, _txBusy) = countMessageTx currentPosition positionChanged

-- =============================================================================
-- Synthesis Top Entity
-- =============================================================================

-- | Top-level entity for FPGA synthesis
-- Configured for Clash synthesis with explicit port names
{-# ANN
  topEntity
  ( Synthesize
      { t_name = "blinky",
        t_inputs =
          [ PortName "CLK", -- System clock (27MHz)
            PortName "RST", -- Reset (active high)
            PortName "EN", -- Enable (active high)
            PortName "BTN1", -- Button 1 input (mode toggle)
            PortName "BTN2", -- Button 2 input (advance)
            PortName "UART_RX" -- UART receive input
          ],
        t_output = PortProduct "" [PortName "LED", PortName "UART_TX"]
      }
  )
  #-}
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  -- | BTN1 (mode toggle)
  Signal System Bit ->
  -- | BTN2 (advance)
  Signal System Bit ->
  -- | UART_RX
  Signal System Bit ->
  -- | (LED output, UART_TX)
  Signal System (Vec 6 Bit, Bit)
topEntity clk rst en btn1 btn2 rxLine =
  withClockResetEnable clk rst en
    $ bundle (mainSystem btn1 btn2 rxLine)

-- =============================================================================
-- Test Functions (for verification)
-- =============================================================================

-- | Test UART receiver with ideal waveform
-- Sends a test string and verifies correct reception
testUartRx :: IO ()
testUartRx = do
  let clockDiv = 234 :: P.Int -- UART baud rate divider
      testMessage = "1A2 \r\n"
      messageBytes = P.map (P.fromIntegral . P.fromEnum) testMessage :: [Word8]

      -- Helper to replicate values
      replicate' x n = P.replicate n x

      -- Encode single byte as UART serial data (8N1 format)
      encodeByte :: P.Int -> Word8 -> [Bit]
      encodeByte divider byte =
        let getBit bitIdx = if B.testBit byte bitIdx then high else low
            dataBits = P.concat [replicate' (getBit j) divider | j <- [0 .. 7]]
            startBit = replicate' low divider
            stopBit = replicate' high divider
         in startBit P.++ dataBits P.++ stopBit

      -- Generate complete test waveform
      rxWaveform = P.concatMap (encodeByte clockDiv) messageBytes
      totalSamples = P.length rxWaveform + 200
      idlePadding = replicate' high 200
      rxSignal = fromList $ P.take totalSamples (rxWaveform P.++ idlePadding)

      -- Run simulation
      (rxData, rxValid) =
        withClockResetEnable @System clockGen resetGen enableGen
          $ uartRx rxSignal

      -- Collect results
      simulationResults = sampleN @System totalSamples (bundle (rxData, rxValid))
      receivedBytes = [P.fromIntegral d :: Word8 | (d, v) <- simulationResults, v]
      receivedString = P.map (P.toEnum . P.fromIntegral) receivedBytes :: [Char]

  -- Display test results
  P.putStrLn "=== UART RX Test Results ==="
  P.putStrLn $ "Expected: " P.++ testMessage
  P.putStrLn $ "Received: " P.++ P.take (P.length testMessage) receivedString
  P.putStrLn $ "Raw bytes: " P.++ P.show receivedBytes
  P.putStrLn $ "Test "
    P.++ if P.take (P.length testMessage) receivedString == testMessage
      then "PASSED"
      else "FAILED"
  P.putStrLn "================================"

-- =============================================================================
-- Main Entry Point
-- =============================================================================

-- | Main entry point for testing and verification
-- Currently runs UART receiver test
main :: IO ()
main = do
  P.putStrLn "=== Clash Tang Nano 20K Test Suite ==="
  P.putStrLn "Testing UART receiver functionality..."
  testUartRx
  P.putStrLn "=== Test Suite Complete ==="