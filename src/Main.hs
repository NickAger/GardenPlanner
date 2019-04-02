module Main where

newtype Plant = Plant { extract :: String } deriving Show

plants =
  [ Plant "potatoes"
  , Plant "onions"
  , Plant "garlic"
  , Plant "parsley"
  , Plant "corrinder"
  , Plant "cucumbers"
  , Plant "beetroot"
  , Plant "spring onions"
  ]

main :: IO ()
main = do
  putStrLn "hello world"
