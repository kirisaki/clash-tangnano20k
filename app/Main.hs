module Main where

import Button
import Clash.Prelude
import LED
import Types
import UART

-- === Integrated System ===

-- Main system (with UART functionality)
mainSystem ::
  (HiddenClockResetEnable dom) =>
  Signal dom Bit -> -- Button 1 raw signal
  Signal dom Bit -> -- Button 2 raw signal
  Signal dom Bit -> -- UART RX
  (Signal dom (Vec 6 Bit), Signal dom Bit) -- (LED output, UART TX)
mainSystem btn1Raw btn2Raw rxLine = (ledOutput, txLine)
  where
    -- Button processing
    (btn1Event, btn2Event) = processButtons btn1Raw btn2Raw

    -- UART receive processing
    (rxData, rxValid) = uartRx rxLine
    (uartBtn1Event, uartBtn2Event) = keyToButtonEvents rxData rxValid

    -- Button event integration (physical buttons + UART key input)
    combinedBtn1Event =
      mux
        (btn1Event .==. pure ButtonPress .||. uartBtn1Event .==. pure ButtonPress)
        (pure ButtonPress)
        (pure NoEvent)
    combinedBtn2Event =
      mux
        (btn2Event .==. pure ButtonPress .||. uartBtn2Event .==. pure ButtonPress)
        (pure ButtonPress)
        (pure NoEvent)

    -- LED control
    (ledOutput, currentPosition, positionChanged) = ledController combinedBtn1Event combinedBtn2Event

    -- UART transmission (count change notification)
    (txLine, _) = countMessageTx currentPosition positionChanged

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "blinky",
        t_inputs =
          [ PortName "CLK",
            PortName "RST",
            PortName "EN",
            PortName "BTN1",
            PortName "BTN2",
            PortName "UART_RX"
          ],
        t_output = PortProduct "" [PortName "LED", PortName "UART_TX"]
      }
  )
  #-}
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Bit -> -- BTN1
  Signal System Bit -> -- BTN2
  Signal System Bit -> -- UART_RX
  Signal System (Vec 6 Bit, Bit) -- (LED, UART_TX)
topEntity clk rst en btn1 btn2 rxLine =
  withClockResetEnable clk rst en
    $ bundle (mainSystem btn1 btn2 rxLine)

main :: IO ()
main = do
  putStrLn "UART-enabled LED Controller simulation:"
  putStrLn "BTN1 or '1'/'A'/'a': Toggle between Auto/Manual mode"
  putStrLn "BTN2 or '2'/Space/Enter: Manual advance (in manual mode)"
  putStrLn "UART sends 'CNT: N' when count changes"

  let btn1Pattern =
        fromList
          [ low,
            low,
            low,
            low,
            low,
            high,
            high,
            high,
            high,
            high,
            low,
            low,
            low,
            low,
            low,
            low,
            low,
            low,
            low,
            low
          ]
  let btn2Pattern =
        fromList
          [ low,
            low,
            low,
            low,
            low,
            low,
            low,
            low,
            high,
            high,
            high,
            high,
            low,
            low,
            low,
            low,
            high,
            high,
            low,
            low
          ]
  let uartRxPattern =
        fromList
          [ high,
            high,
            high,
            high,
            high,
            high,
            high,
            high,
            high,
            high,
            high,
            high,
            high,
            high,
            high,
            high,
            high,
            high,
            high,
            high -- UART idle (high)
          ]

  let result =
        sampleN @System 20
          $ withClockResetEnable @System clockGen resetGen enableGen
          $ bundle (mainSystem btn1Pattern btn2Pattern uartRxPattern)

  putStrLn "Simulation results (LED, UART_TX):"
  mapM_ print result