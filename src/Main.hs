module Main where

newtype Ribbon = Ribbon [Bool] Int

newtype RawData = RawData [Ribbon]

newtype Limits = Limits { limitsLow :: Int, limitsHigh :: Int }

drawSegment :: Limits -> Bool -> Bool -> Int -> String
drawSegment (Limits _ h) True True i =
  show (i,h) <> " -- " <> show ((fromInteger i + 1.0) / 2.0, h) <> " -- " show (i+1,h)

drawRibbon :: Ribbon -> String
drawRibbon (Ribbon b i) =


main :: IO ()
main = putStrLn "Hello, Haskell!"
