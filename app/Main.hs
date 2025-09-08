module Main where

-- 設計側（Clash）

import Button
import Clash.Prelude
import Control.Monad (when)
import qualified Data.Bits as B
import Data.Word
import LED
import Types
import UART
import qualified Prelude as P

--------------------------------------------------------------------------------
-- Integrated System
--------------------------------------------------------------------------------

-- Btn1: モード切替 (Auto ↔ Manual)
-- Btn2: 1つ進む（Manual時のみ有効。Auto時はtickで自動進行）
mainSystem ::
  (HiddenClockResetEnable dom) =>
  Signal dom Bit -> -- BTN1 raw (active-low想定, モード切替)
  Signal dom Bit -> -- BTN2 raw (active-low想定, 進む)
  Signal dom Bit -> -- UART RX
  ( Signal dom (Vec 6 Bit), -- LED
    Signal dom Bit -- UART TX
  )
mainSystem btn1Raw btn2Raw rxLine = (ledOutput, txLine)
  where
    -- 物理ボタン処理
    (btn1Event, btn2Event) = processButtons btn1Raw btn2Raw

    -- UART 受信
    (rxData, rxValid) = uartRx rxLine

    -- RXからキー入力をButtonイベントに変換
    (uartBtn1Event, uartBtn2Event) = keyToButtonEvents rxData rxValid

    -- ボタンとUART入力を合流
    btn1Event' = orButtonEventS btn1Event uartBtn1Event
    btn2Event' = orButtonEventS btn2Event uartBtn2Event

    -- LED制御
    (ledOutput, currentPosition, positionChanged) =
      ledController btn1Event' btn2Event'

    -- 位置変更時に送信（CNT: n\r\n）
    (txLine, _busy) = countMessageTx currentPosition positionChanged

--------------------------------------------------------------------------------
-- UART RX 単体テスト（理想波形）
--------------------------------------------------------------------------------
testUartRx :: IO ()
testUartRx = do
  let div = 234 :: P.Int
      msgStr = "1A2 \r\n"
      msg = P.map (P.fromIntegral . P.fromEnum) msgStr :: [Word8]

      rep x n = P.replicate n x

      encodeByte :: P.Int -> Word8 -> [Bit]
      encodeByte d w =
        let bit j = if B.testBit w j then high else low
            dataBits = P.concat [rep (bit j) d | j <- [0 .. 7]]
         in rep low d P.++ dataBits P.++ rep high d

      rxLineList = P.concatMap (encodeByte div) msg
      nSamples = P.length rxLineList + 200
      rxSig = fromList (P.take nSamples (rxLineList P.++ rep high 200))

      (rxData, rxValid) =
        withClockResetEnable @System clockGen resetGen enableGen
          $ uartRx rxSig

      samples = sampleN @System nSamples (bundle (rxData, rxValid))
      gotBytes = [P.fromIntegral d :: Word8 | (d, v) <- samples, v]
      gotStr = P.map (P.toEnum . P.fromIntegral) gotBytes :: [Char]

  P.putStrLn "=== UART RX test (ideal waveform) ==="
  P.putStrLn ("Expect: " P.++ msgStr)
  P.putStrLn ("Got   : " P.++ P.take (P.length msgStr) gotStr)
  P.putStrLn ("Bytes : " P.++ P.show gotBytes)
  P.putStrLn "-----"

--------------------------------------------------------------------------------
-- Synthesis top
--------------------------------------------------------------------------------
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
  Signal System Bit -> -- BTN1 (mode toggle)
  Signal System Bit -> -- BTN2 (advance)
  Signal System Bit -> -- UART_RX
  Signal System (Vec 6 Bit, Bit) -- (LED, UART_TX)
topEntity clk rst en btn1 btn2 rxLine =
  withClockResetEnable clk rst en
    $ bundle (mainSystem btn1 btn2 rxLine)

--------------------------------------------------------------------------------
-- Main entry (切り替えてテスト)
--------------------------------------------------------------------------------
main :: IO ()
main = do
  P.putStrLn "===== Button-Only Debug Suite (RX idle) ====="
  testUartRx
  -- 他のテストはコメント解除で
  -- testUartTx
  -- testBtn1ModeToggle
  -- testBtn2Advance
  -- testBtn1Then2
  P.putStrLn "===== Test Suite Complete ====="
