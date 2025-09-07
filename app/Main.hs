module Main where

import Clash.Prelude

-- LED順次点灯回路
ledSequencer :: (HiddenClockResetEnable dom) => Signal dom (Vec 6 Bit)
ledSequencer = invertBits . vecToBits <$> ledPattern
  where
    tick = riseEvery (SNat @13_500_000)

    counter = regEn (0 :: Unsigned 3) tick ((\x -> (x + 1) `mod` 6) <$> counter)

    ledPattern = vecFromIndex <$> counter

    vecFromIndex :: Unsigned 3 -> Vec 6 Bool
    vecFromIndex idx = replace idx True (repeat False)

    vecToBits :: Vec 6 Bool -> Vec 6 Bit
    vecToBits = map boolToBit

    invertBits :: Vec 6 Bit -> Vec 6 Bit
    invertBits = map complement

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
  Signal System (Vec 6 Bit)
topEntity clk rst en = withClockResetEnable clk rst en ledSequencer

main :: IO ()
main = do
  putStrLn "LED Sequencer simulation (first 30 cycles):"
  print (sampleN @System 30 (withClockResetEnable @System clockGen resetGen enableGen ledSequencer))