module Main where

import Clash.Prelude

-- === カスタム型定義 ===

-- 動作モード
data Mode = Auto | Manual
  deriving (Generic, NFDataX, Eq, Show)

-- LED位置
data LedPosition = Led0 | Led1 | Led2 | Led3 | Led4 | Led5
  deriving (Generic, NFDataX, Eq, Show, Enum, Bounded)

-- ボタン状態
data ButtonState = Pressed | Released
  deriving (Generic, NFDataX, Eq, Show)

-- ボタンイベント
data ButtonEvent = ButtonPress | ButtonRelease | NoEvent
  deriving (Generic, NFDataX, Eq, Show)

-- LED制御コマンド
data LedCommand = Advance | Hold
  deriving (Generic, NFDataX, Eq, Show)

-- === ユーティリティ関数 ===

-- モード切り替え
toggleMode :: Mode -> Mode
toggleMode Auto = Manual
toggleMode Manual = Auto

-- LED位置の次へ移動
nextLedPosition :: LedPosition -> LedPosition
nextLedPosition Led5 = Led0
nextLedPosition pos = succ pos

-- LED位置をビットパターンに変換
ledPositionToBits :: LedPosition -> Vec 6 Bit
ledPositionToBits pos = invertBits $ vecToBits $ vecFromIndex (fromEnum pos)
  where
    vecFromIndex :: Int -> Vec 6 Bool
    vecFromIndex idx = replace (fromIntegral idx) True (repeat False)

    vecToBits :: Vec 6 Bool -> Vec 6 Bit
    vecToBits = map boolToBit

    invertBits :: Vec 6 Bit -> Vec 6 Bit
    invertBits = map complement

-- === ハードウェア記述 ===

-- ボタンデバウンス（抽象型使用）
buttonDebounce :: (HiddenClockResetEnable dom) => Signal dom Bit -> Signal dom ButtonState
buttonDebounce btnRaw = debounced
  where
    debounceTime = SNat @1_350_000
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

-- ボタンイベント検出
buttonEventDetector :: (HiddenClockResetEnable dom) => Signal dom ButtonState -> Signal dom ButtonEvent
buttonEventDetector btnState = event
  where
    prev1 = register Released btnState
    prev2 = register Released prev1

    -- 安定した状態変化を検出
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

-- LED制御ロジック
ledController ::
  (HiddenClockResetEnable dom) =>
  Signal dom ButtonEvent -> -- ボタン1イベント
  Signal dom ButtonEvent -> -- ボタン2イベント
  Signal dom (Vec 6 Bit)
ledController btn1Event btn2Event = ledOutput
  where
    -- モード管理
    modeChangeEvent = btn1Event .==. pure ButtonPress
    mode = regEn Auto modeChangeEvent (toggleMode <$> mode)

    -- LED制御コマンド生成
    tick = riseEvery (SNat @27_000_000) -- 1秒間隔
    manualAdvance = btn2Event .==. pure ButtonPress

    ledCommand =
      mux
        (mode .==. pure Auto)
        (mux tick (pure Advance) (pure Hold)) -- Autoモード
        (mux manualAdvance (pure Advance) (pure Hold)) -- Manualモード

    -- LED位置管理
    ledAdvanceEvent = ledCommand .==. pure Advance
    ledPosition = regEn Led0 ledAdvanceEvent (nextLedPosition <$> ledPosition)

    -- LED出力生成
    ledOutput = ledPositionToBits <$> ledPosition

-- トップレベル統合
ledSystem ::
  (HiddenClockResetEnable dom) =>
  Signal dom Bit -> -- ボタン1生信号
  Signal dom Bit -> -- ボタン2生信号
  Signal dom (Vec 6 Bit)
ledSystem btn1Raw btn2Raw = ledController btn1Event btn2Event
  where
    -- ボタン処理パイプライン
    btn1State = buttonDebounce btn1Raw
    btn2State = buttonDebounce btn2Raw
    btn1Event = buttonEventDetector btn1State
    btn2Event = buttonEventDetector btn2State

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
  Signal System Bit ->
  Signal System Bit ->
  Signal System (Vec 6 Bit)
topEntity clk rst en btn1 btn2 = withClockResetEnable clk rst en (ledSystem btn1 btn2)

main :: IO ()
main = do
  putStrLn "Abstract LED Controller simulation:"
  putStrLn "BTN1: Toggle between Auto/Manual mode"
  putStrLn "BTN2: Manual advance (in manual mode)"

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