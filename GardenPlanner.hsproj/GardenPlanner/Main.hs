import Data.List
import Control.Monad

data PlantType = Potatoes | Onions | Garlic | Parsley | Cariander | Cucumber | Beetroot | SpringOnions | SweetCorn | Carrots | Leaks | FrenchBeans | Peas | Dill | Radsh deriving (Eq, Show)

data BedProportion = Quarter | Half | Eighth | Tenth deriving (Eq, Show)

data AphidStatus = Susceptible | Repellant | Neutral  deriving (Eq, Show)

data Plant = Plant { name :: PlantType
                   , size :: BedProportion
                   , aphidStatus :: AphidStatus
                   , badNeighbours :: [PlantType]
                   } deriving (Eq, Show)


plants =
  [ Plant Potatoes Half Neutral [Onions]
  , Plant Onions Quarter Neutral [Garlic]
  , Plant Garlic Eighth Repellant [Onions]
  , Plant Parsley Eighth Neutral []
  , Plant Cariander Quarter Neutral []
  , Plant Cucumber Half Neutral []
  , Plant Beetroot Quarter Neutral []
  , Plant SpringOnions Quarter Neutral []
  , Plant SweetCorn Half Neutral []
  , Plant Carrots Quarter Susceptible {-Repellant-} []
  , Plant Leaks Quarter Neutral []
  , Plant FrenchBeans Eighth Neutral []
  , Plant Peas Eighth Neutral []
  , Plant Dill Tenth Neutral []
  , Plant Radsh Tenth Neutral []
  ]

valueToBedProportion Quarter = 2.5
valueToBedProportion Half = 5
valueToBedProportion Eighth = 1.25
valueToBedProportion Tenth = 1


--

isAphidSafe plants = 
  let 
    anySusceptible = any ((== Susceptible)  . aphidStatus) plants
    anyRepellant = any ((== Repellant)  . aphidStatus) plants
  in
    (anySusceptible && anyRepellant) || not anySusceptible

areGoodNeighbours plants =
  let
    allBadNeighbours = nub $ concat $ map badNeighbours plants
    plantNames = map name plants
  in
    noMatchingElements plantNames allBadNeighbours

plantsTotal plants = sum $ map (valueToBedProportion . size) plants

isFull :: [Plant] -> Bool
isFull plants = 
  let 
    existingTotal = plantsTotal plants
  in
    existingTotal > 8 && existingTotal < 14
  
--

mySubsequences :: [a] -> [[a]]
mySubsequences as = do
  t <- tails as
  n <- [0..9]
  let front = take n t
  let rest = drop n t
  r <- rest
  let c = front ++ [r]
  guard $ length c > 1
  return $ c

plantCombinations2 :: [Plant] -> [[Plant]]
plantCombinations2 ps = [c |t <- tails ps, n <- [1..9], let front = take n t, let rest = drop n t, r <- rest, let c = front ++ [r], isFull c]

plantCombinations :: [Plant] -> [[Plant]]
plantCombinations plants = do
  t <- tails plants
  n <- [0..9]
  let front = take n t
  let rest = drop n t
  r <- rest
  let c = front ++ [r]
  guard $ length c > 1 && isFull c && areGoodNeighbours c && isAphidSafe c
  return c
  

plantCombinations3 :: [Plant] -> [[Plant]]
plantCombinations3 plants = filter (\c -> length c > 1 && isFull c && areGoodNeighbours c && isAphidSafe c) $ subsequences plants
  
--
 
bedCombinations = 
  [(x, y, z) | let pc = plantCombinations3 plants, x <- pc, y <- pc, z <- pc, {-allUnique $ x ++ y ++ z-} {-noMatchingElements x y, noMatchingElements x z, noMatchingElements y z-}allUnique2 x y z]
  

--

displayCombinations cs = do
  let indexedCs = zip cs [1..]
  mapM_  displayCombinationIndex indexedCs
  
displayCombinationIndex (c, i) = do
  putStrLn $ "Combination " ++ show i
  displayBedCombination c
  
displayBedCombination (a, b, c) = do
  displayBed "bed a: " a
  displayBed "bed b: " b
  displayBed "bed c: " c    

displayBed title plants =
  let 
    descriptions = map (show . name) plants
    description = (concat $ intersperse ", " descriptions) ++ " (" ++ (show $ plantsTotal plants) ++ ")"
  in    
    putStrLn $ title ++ description 
    
--

noMatchingElements :: Eq a => [a] -> [a] -> Bool
noMatchingElements xs ys = all id (map (\x -> all (/=x) ys) xs)

-- also implemented in `Data.List.Unique` 
-- ... but HFM wouldn't load `Unique` package
allUnique :: Eq a => [a] -> Bool
allUnique list = (length $ nub list) == length list

allUnique2 :: Eq a => [a] -> [a] -> [a] -> Bool
allUnique2 bedA bedB bedC = ((length bedA) + (length bedB) + (length bedC)) == unionLength
  where
     unionLength = length $ union bedC $ union bedA bedB 
