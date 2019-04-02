module Main where

import           Data.List
import           Control.Monad

data PlantType = Potatoes | Onions | Garlic | Parsley | Cariander | Cucumber | Beetroot | SpringOnions | SweetCorn | Carrots | Leaks | FrenchBeans | Peas | Dill | Radsh deriving (Eq, Show)

data BedProportion = Quarter | Half | Eighth | Tenth deriving (Eq, Show)

data AphidStatus = Susceptible | Repellant | Neutral  deriving (Eq, Show)

data Plant = Plant { name :: PlantType
                   , size :: BedProportion
                   , aphidStatus :: AphidStatus
                   , badNeighbours :: [PlantType]
                   } deriving (Eq, Show)


plants =
  [ Plant Potatoes     Half    Neutral     [Onions]
  , Plant Onions       Quarter Neutral     [Garlic]
  , Plant Garlic       Eighth  Repellant   [Onions]
  , Plant Parsley      Eighth  Neutral     []
  , Plant Cariander    Quarter Neutral     []
  , Plant Cucumber     Half    Neutral     []
  , Plant Beetroot     Quarter Neutral     []
  , Plant SpringOnions Quarter Neutral     []
  , Plant SweetCorn    Half    Neutral     []
  , Plant Carrots      Quarter Susceptible []
  , Plant Leaks        Quarter Neutral     []
  , Plant FrenchBeans  Eighth  Neutral     []
  , Plant Peas         Eighth  Neutral     []
  , Plant Dill         Tenth   Neutral     []
  , Plant Radsh        Tenth   Neutral     []
  ]

valueToBedProportion Quarter = 2.5
valueToBedProportion Half    = 5
valueToBedProportion Eighth  = 1.25
valueToBedProportion Tenth   = 1


--

isAphidSafe plants =
  let anySusceptible = any ((== Susceptible) . aphidStatus) plants
      anyRepellant   = any ((== Repellant) . aphidStatus) plants
  in  (anySusceptible && anyRepellant) || not anySusceptible

areGoodNeighbours plants =
  let allBadNeighbours = nub $ concatMap badNeighbours plants
      plantNames       = map name plants
  in  noMatchingElements plantNames allBadNeighbours

plantsTotal plants = sum $ map (valueToBedProportion . size) plants

isFull :: [Plant] -> Bool
isFull plants = let existingTotal = plantsTotal plants in existingTotal > 8 && existingTotal < 14

noMatchingElements :: Eq a => [a] -> [a] -> Bool
noMatchingElements xs ys = unionLength == (length xs) + (length ys) where unionLength = length $ union xs xs
--

plantCombinations :: [Plant] -> [[Plant]]
plantCombinations plants =
  filter (\c -> length c > 1 && isFull c && areGoodNeighbours c && isAphidSafe c) $ subsequences plants

--

bedCombinations = [ (x, y, z) | let pc = plantCombinations plants, x <- pc, y <- pc, z <- pc, allUnique x y z ]

allUnique :: Eq a => [a] -> [a] -> [a] -> Bool
allUnique bedA bedB bedC = ((length bedA) + (length bedB) + (length bedC)) == unionLength
  where unionLength = length $ union bedC $ union bedA bedB

--

displayCombinations cs = do
  let indexedCs = zip cs [1 ..]
  mapM_ displayCombinationIndex indexedCs

displayCombinationIndex (c, i) = do
  putStrLn $ "Combination " ++ show i
  displayBedCombination c

displayBedCombination (a, b, c) = do
  displayBed "bed a: " a
  displayBed "bed b: " b
  displayBed "bed c: " c

displayBed title plants =
  let descriptions = map (show . name) plants
      description  = (concat $ intersperse ", " descriptions) ++ " (" ++ (show $ plantsTotal plants) ++ ")"
  in  putStrLn $ title ++ description

--

main = do
  let c = bedCombinations
  putStrLn $ "number of bed combinations = " ++ (show $ length c)
  -- displayCombinations bedCombinations

