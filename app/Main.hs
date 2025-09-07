{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Clash.Prelude

blinker :: (HiddenClockResetEnable dom) => Signal dom Bit
blinker = boolToBit <$> led
  where
    tick = riseEvery (SNat @13_500_000)
    led = regEn False tick (Clash.Prelude.not <$> led) -- T-FF

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "blinky",
        t_inputs = [PortName "CLK", PortName "RST", PortName "EN"],
        t_output = PortName "LED"
      }
  )
  #-}
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Bit
topEntity clk rst en = withClockResetEnable clk rst en blinker

main :: IO ()
main =
  print (sampleN @System 20 (withClockResetEnable @System clockGen resetGen enableGen blinker))
