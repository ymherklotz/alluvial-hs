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

incrX :: Double -> (Double, [Coord]) -> (Limits, Bool) -> (Double, [Coord])
incrX len (d, coords) (l, b) = (d + len, boolToCoords l d b : coords)

drawRibbon :: Double -> [Limits] -> [Bool] -> [Coord]
drawRibbon len l b =
  snd $ foldl (incrX len) (0, []) $ zip l b

drawCoords :: String -> [Coord] -> String
drawCoords connection coords =
  mconcat $ List.intersperse connection (show <$> coords)

inBetween :: Coord -> Coord -> Coord
inBetween (Coord x1 y1) (Coord x2 y2) =
  Coord ((x1 + x2) / 2) ((y1 + y2) / 2)

condOp :: (Double -> Double) -> [Bool] -> [Double] -> [Double]
condOp f b d =
  fmap condOp' $ zip b d
  where
    condOp' (True, d') = f d'
    condOp' (False, d') = d'

condNotOp :: (Double -> Double) -> [Bool] -> [Double] -> [Double]
condNotOp f b d =
  fmap condOp' $ zip b d
  where
    condOp' (False, d') = f d'
    condOp' (True, d') = d'

drawArrow ::
  Double ->
  Double ->
  (Int, [Double], [Double], String) ->
  Ribbon ->
  (Int, [Double], [Double], String)
drawArrow ratio len (it, start, end, s) (Ribbon b i) =
  ( it + 1,
    condOp (\x -> x - i) b start,
    condNotOp (\x -> x - i) b end,
    s
      <> "\\fill[ribbon"
      <> show it
      <> "] "
      <> (drawCoords " to [out=0,in=180] " . reverse $ drawRibbon len upper_limit b)
      <> " -- "
      <> drawCoords
        " -- "
        [ mid1,
          moveX (i * ratio) (inBetween mid1 mid2),
          mid2
        ]
      <> " -- "
      <> (drawCoords " to [out=180,in=0] " $ drawRibbon len lower_limit b)
      <> ";\n"
  )
  where
    upper_limit =
      fmap
        ( \(st, e) ->
            Limits (e * ratio) (st * ratio)
        )
        $ zip start end
    lower_limit =
      fmap
        ( \(st, e) ->
            Limits ((e - i) * ratio) ((st - i) * ratio)
        )
        $ zip start end
    mid1 = boolToCoords (last upper_limit) ((fromIntegral $ length b) * len) (last b)
    mid2 = boolToCoords (last lower_limit) ((fromIntegral $ length b) * len) (last b)

fourth :: (a, b, c, d) -> d
fourth (_, _, _, d) = d

toListLength :: [Double] -> Ribbon -> [Double]
toListLength d (Ribbon r i) =
  fmap boolToDouble (zip d r)
  where
    boolToDouble (d', True) = d' + i
    boolToDouble (d', False) = d'

initColumns :: Double -> [Ribbon] -> [Double]
initColumns d (Ribbon r _ : _) = replicate (length r) d
initColumns _ _ = []

calcMaxColumns :: [Ribbon] -> [Double]
calcMaxColumns rs = foldl toListLength (initColumns 0 rs) rs

drawRibbons :: Double -> Double -> [Ribbon] -> String
drawRibbons ratio len rs =
  fourth $ foldl (drawArrow ratio len) (1, total, ends, "") rs
  where
    total = initColumns ((+ (0.5 / ratio)) . sum $ fmap (\(Ribbon _ d) -> d) rs) rs
    ends = fmap (\(m, t) -> t - m - 0.5 / ratio) $ zip (calcMaxColumns rs) total

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
printAll = putStrLn $ drawRibbons 0.05 2.0 fpga2020

main :: IO ()
main = putStrLn "Hello, Haskell!"
