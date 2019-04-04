module Main where

import           Data.List
import           Control.Monad

data PlantType = Potatoes | Onions | Garlic | Parsley | Coriander | Cucumber | Beetroot | SpringOnions | SweetCorn | Carrots | Leaks | FrenchBeans | Peas | Dill | Radish deriving (Eq, Show)
data BedProportion = Quarter | Half | Eighth | Tenth deriving (Eq, Show)
data AphidStatus = Susceptible | Repellant | Neutral  deriving (Eq, Show)
data Plant = Plant { name :: PlantType
                   , size :: BedProportion
                   , aphidStatus :: AphidStatus
                   , badNeighbours :: [PlantType]
                   } deriving (Eq, Show)

type BedPlants = [Plant]

plants =
  [ Plant Potatoes     Half    Neutral     [Onions]
  , Plant Onions       Quarter Neutral     [Garlic]
  , Plant Garlic       Eighth  Repellant   [Onions]
  , Plant Parsley      Eighth  Neutral     []
  , Plant Coriander    Quarter Neutral     []
  , Plant Cucumber     Half    Neutral     []
  , Plant Beetroot     Quarter Neutral     []
  , Plant SpringOnions Quarter Neutral     []
  , Plant SweetCorn    Half    Neutral     []
  , Plant Carrots      Quarter Susceptible []
  , Plant Leaks        Quarter Neutral     []
  , Plant FrenchBeans  Eighth  Neutral     []
  , Plant Peas         Eighth  Neutral     []
  , Plant Dill         Tenth   Neutral     []
  , Plant Radish       Tenth   Neutral     []
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

isFull :: BedPlants -> Bool
isFull plants = let existingTotal = plantsTotal plants in existingTotal > 8 && existingTotal < 13

noMatchingElements :: Eq a => [a] -> [a] -> Bool
noMatchingElements xs ys = unionLength == length xs + length ys where unionLength = length $ union xs ys
--

plantCombinations :: [Plant] -> [BedPlants]
plantCombinations plants =
  filter (\c -> length c > 1 && isFull c && areGoodNeighbours c && isAphidSafe c) $ subsequences plants

--

{-
Gets the combinations for the first bed, subtracts from the available plants to find the plants not in the first bed, then given
those plants generates the combinations for the second bed. So a single first bed combination generates multiple second bed 
combinations. Repeats for the third bed.
Algorithmn thanks to Stephen.
Much more efficient than brute-forcing with a uniqueness check, eg:
```
bedCombinations = [ (x, y, z) | let pc = plantCombinations plants, x <- pc, y <- pc, z <- pc, allUnique x y z ]
```
-}
bedCombinations :: [(BedPlants, BedPlants, BedPlants)]
bedCombinations =
  let x :: [BedPlants]
      x = plantCombinations plants

      y :: [(BedPlants, BedPlants)]
      y = concat $ combine <$> x
  in  concat $ combine2 <$> y

 where
  combine :: BedPlants -> [(BedPlants, BedPlants)]
  combine p = (,) <$> pure p <*> plantCombinations (plants \\ p)

  combine2 :: (BedPlants, BedPlants) -> [(BedPlants, BedPlants, BedPlants)]
  combine2 (p, q) = (\c -> (p, q, c)) <$> plantCombinations (plants \\ union p q)

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

containsPlant :: PlantType -> (BedPlants, BedPlants, BedPlants) -> Bool
containsPlant plantType (bedA, bedB, bedC) =
  let bedContainsSpecificPlant = bedContainsPlant plantType
  in  bedContainsSpecificPlant bedA || bedContainsSpecificPlant bedB || bedContainsSpecificPlant bedC
 where
  bedContainsPlant :: PlantType -> BedPlants -> Bool
  bedContainsPlant plantType = any ((== plantType) . name)

--

-- defined as the combinations with the largest number of plants
bestCombinations :: [(BedPlants, BedPlants, BedPlants)] -> [(BedPlants, BedPlants, BedPlants)]
bestCombinations combinations =
  let scoreAdded         = map (\c@(bedA, bedB, bedC) -> (numberOfPlantsInCombination c, bedA, bedB, bedC)) combinations
      maximumTotalPlants = foldr maximumLength 0 scoreAdded
      topScores          = filter ((== maximumTotalPlants) . fst4) scoreAdded
  in  map (\(_, bedA, bedB, bedC) -> (bedA, bedB, bedC)) topScores
 where
  fst4 (a, b, c, d) = a
  maximumLength :: (Int, BedPlants, BedPlants, BedPlants) -> Int -> Int
  maximumLength scoredCombination maxSoFar =
    let length1 = fst4 scoredCombination in if length1 >= maxSoFar then length1 else maxSoFar

numberOfPlantsInCombination (bedA, bedB, bedC) = length bedA + length bedB + length bedC

--

main = do
  let pc = plantCombinations plants
  putStrLn $ "Number of plant combinations in one bed = " ++ show (length pc)

  let c = bedCombinations
  putStrLn $ "Number of bed combinations = " ++ show (length c)

  -- let anyPotatoes = any (containsPlant Potatoes) c
  -- putStrLn $ "anyPotatoes = " ++ show anyPotatoes

  -- let anyCucumber = any (containsPlant Cucumber) c
  -- putStrLn $ "anyCucumber = " ++ show anyCucumber

  -- let firstTwoCombinations = take 2 c
  -- displayCombinations firstTwoCombinations
  let bestBeds = bestCombinations c
  putStrLn $ "Number of plants to combine = " ++ show (length plants)
  putStrLn $ "Number of plants in best beds = " ++ show (numberOfPlantsInCombination $ head bestBeds)
  putStrLn $ "Number of beds with largest number of plants = " ++ show (length bestBeds)

  putStrLn "\nBeds with largest combinations of plants (first two):"
  displayCombinations $ take 2 bestBeds

