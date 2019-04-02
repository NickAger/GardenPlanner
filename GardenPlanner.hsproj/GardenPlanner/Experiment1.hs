module Experiment1 where

import Data.List

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]


noMatchingElements x y = 
  fst x /= fst y && fst x /= snd y && snd x /= snd y && snd x /= fst y


newtype Plant = Plant String deriving (Eq, Show)

plants =
  [ Plant "potatoes"
  , Plant "onions"
  , Plant "garlic"
  , Plant "parsley"
  , Plant "corrinder"
  , Plant "cucumbers"
  , Plant "beetroot"
  , Plant "spring onions"
  , Plant "sweetcorn"
  ]
  
plantPairs = pairs plants

plantCombinations = 
  [(x, y, z) | x <- pairs plants, y <- pairs plants, z <- pairs plants, noMatchingElements x y, noMatchingElements x z, noMatchingElements y z]
  
displayCombinations cs = do
  let indexedCs = zip cs [1..]
  mapM_  displayCombinationIndex indexedCs
  
displayCombinationIndex (c, i) = do
  putStrLn $ "Combination " ++ show i
  displayCombination c
  
displayCombination (a, b, c) = do
  displayBed "bed a: " a
  displayBed "bed b: " b
  displayBed "bed c: " c    

displayBed description (Plant x, Plant y)  = 
  putStrLn $ description ++ x ++ ", " ++ y