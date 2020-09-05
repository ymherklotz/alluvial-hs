module Main where

import qualified Data.List as List

data Ribbon = Ribbon [Bool] Double
  deriving (Eq, Show)

newtype RawData = RawData [Ribbon]
  deriving (Eq, Show)

data Limits = Limits {limitsLow :: Double, limitsHigh :: Double}
  deriving (Eq, Show)

data Coord = Coord {coordX :: Double, coordY :: Double}
  deriving (Eq)

instance Show Coord where
  show (Coord x y) = "(" <> show x <> "," <> show y <> ")"

boolToCoords :: Limits -> Double -> Bool -> Coord
boolToCoords (Limits _ h) x True = Coord x h
boolToCoords (Limits l _) x False = Coord x l

moveX :: Double -> Coord -> Coord
moveX d (Coord x y) = Coord (x + d) y

incrX :: Limits -> Double -> (Double, [Coord]) -> Bool -> (Double, [Coord])
incrX l len (d, coords) b = (d + len, boolToCoords l d b : coords)

drawRibbon :: Limits -> Double -> [Bool] -> [Coord]
drawRibbon l len b =
  snd $ foldl (incrX l len) (0, []) b

drawCoords :: String -> [Coord] -> String
drawCoords connection coords =
  mconcat $ List.intersperse connection (show <$> coords)

inBetween :: Coord -> Coord -> Coord
inBetween (Coord x1 y1) (Coord x2 y2) =
  Coord ((x1 + x2) / 2) ((y1 + y2) / 2)

drawArrow :: Double -> Double -> Double -> (Double, String) -> Ribbon -> (Double, String)
drawArrow ratio len maxVal (start, s) (Ribbon b i) =
  ( start - i,
    s
      <> "\\draw "
      <> (drawCoords " to [out=0,in=180] " . reverse $ drawRibbon upper_limit len b)
      <> " -- "
      <> drawCoords
        " -- "
        [ mid1,
          moveX (len / 4) (inBetween mid1 mid2),
          mid2
        ]
      <> " -- "
      <> (drawCoords " to [out=180,in=0] " $ drawRibbon lower_limit len b)
      <> ";\n"
  )
  where
    upper_limit = Limits ((start - maxVal) * ratio) (start * ratio)
    lower_limit = Limits ((start - i - maxVal) * ratio) ((start - i) * ratio)
    mid1 = boolToCoords upper_limit ((fromIntegral $ length b) * len) (last b)
    mid2 = boolToCoords lower_limit ((fromIntegral $ length b) * len) (last b)

drawRibbons :: Double -> Double -> [Ribbon] -> String
drawRibbons ratio len rs =
  snd $ foldl (drawArrow ratio len total) (total, "") rs
  where
    total = sum $ map (\(Ribbon _ y) -> y) rs

fpga2020 :: [Ribbon]
fpga2020 =
  [ Ribbon [True, True, True, True] 15,
    Ribbon [True, True, True, False] 1,
    Ribbon [True, True, False, False] 6,
    Ribbon [False, False, True, True] 11,
    Ribbon [False, False, True, False] 1,
    Ribbon [False, False, False, True] 17
  ]

printAll :: IO ()
printAll = putStrLn $ drawRibbons 0.05 3.0 fpga2020

main :: IO ()
main = putStrLn "Hello, Haskell!"
