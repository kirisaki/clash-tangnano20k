module Main where

import Clash.Prelude

-- より安定したボタンデバウンス回路（約50ms）
buttonDebounce :: (HiddenClockResetEnable dom) => Signal dom Bit -> Signal dom Bool
buttonDebounce btnRaw = debounced
  where
    -- 27MHz / 1_350_000 ≈ 50ms（より長いデバウンス時間）
    debounceTime = SNat @1_350_000

    -- ボタンの状態を安定させるためのカウンター
    counter = regEn (0 :: Unsigned 21) (pure True) nextCounter

    -- ボタンが押されている（high）間はカウントアップ、そうでなければリセット
    nextCounter =
      mux
        (btnRaw .==. pure high)
        (satAdd SatBound <$> counter <*> pure 1)
        (pure 0)

    -- カウンターが閾値を超えたらボタンが安定して押されたと判定
    debounced = counter .>=. pure (snatToNum debounceTime)

-- より安定したエッジ検出（2段階の同期化）
risingEdge :: (HiddenClockResetEnable dom) => Signal dom Bool -> Signal dom Bool
risingEdge sig = edge
  where
    -- 2段階の同期化でより安定したエッジ検出
    prev1 = register False sig
    prev2 = register False prev1
    edge = liftA2 (&&) sig (liftA2 (&&) (not <$> prev1) (not <$> prev2))

-- LEDコントロール回路（位置保持版）
ledController ::
  (HiddenClockResetEnable dom) =>
  Signal dom Bool -> -- ボタン1の立ち上がりエッジ
  Signal dom Bool -> -- ボタン2の立ち上がりエッジ
  Signal dom (Vec 6 Bit)
ledController btn1Edge btn2Edge = ledOutput
  where
    -- モード管理（False: 自動点滅, True: 手動制御）
    mode = regEn False btn1Edge (not <$> mode)

    -- 自動点滅用のティック
    tick = riseEvery (SNat @27_000_000) -- 約1秒

    -- 共通のLEDカウンター（位置を保持）
    -- 自動モードではtickで更新、手動モードではbtn2Edgeで更新
    updateEnable = mux mode btn2Edge tick
    ledCounter = regEn (0 :: Unsigned 3) updateEnable ((\x -> (x + 1) `mod` 6) <$> ledCounter)

    selectedCounter = ledCounter

    -- LEDパターンを生成
    ledPattern = vecFromIndex <$> selectedCounter
    ledOutput = invertBits . vecToBits <$> ledPattern

    vecFromIndex :: Unsigned 3 -> Vec 6 Bool
    vecFromIndex idx = replace idx True (repeat False)

    vecToBits :: Vec 6 Bool -> Vec 6 Bit
    vecToBits = map boolToBit

    invertBits :: Vec 6 Bit -> Vec 6 Bit
    invertBits = map complement

-- トップレベル回路
ledSystem ::
  (HiddenClockResetEnable dom) =>
  Signal dom Bit -> -- ボタン1生信号
  Signal dom Bit -> -- ボタン2生信号
  Signal dom (Vec 6 Bit)
ledSystem btn1Raw btn2Raw = ledController btn1Edge btn2Edge
  where
    -- デバウンス処理
    btn1Clean = buttonDebounce btn1Raw
    btn2Clean = buttonDebounce btn2Raw

    -- エッジ検出
    btn1Edge = risingEdge btn1Clean
    btn2Edge = risingEdge btn2Clean

{-# ANN
  topEntity
  ( Synthesize
      { t_name = "blinky",
        t_inputs = [PortName "CLK", PortName "RST", PortName "EN", PortName "BTN1", PortName "BTN2"],
        t_output = PortName "LED"
      }
  )
  #-}
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Bit -> -- Button 1 input
  Signal System Bit -> -- Button 2 input
  Signal System (Vec 6 Bit)
topEntity clk rst en btn1 btn2 = withClockResetEnable clk rst en (ledSystem btn1 btn2)

main :: IO ()
main = do
  putStrLn "LED Controller with Button Input simulation (stable version):"
  putStrLn "BTN1: Toggle between auto/manual mode"
  putStrLn "BTN2: Manual advance (in manual mode)"
  -- Button input patterns for simulation
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
  print
    ( sampleN @System
        20
        ( withClockResetEnable @System
            clockGen
            resetGen
            enableGen
            (ledSystem btn1Pattern btn2Pattern)
        )
    )