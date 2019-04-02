module ListGeneration where
  
import Data.List
  
pairs :: [a] -> [[a]]
pairs l = [a:[r] | (a:rest) <- tails l, r <- rest]

threes :: [a] -> [[a]]
threes l = [a:b:[r] | (a:b:rest) <- tails l, r <- rest]

fours :: [a] -> [[a]]
fours l = [a:b:c:[r] | (a:b:c:rest) <- tails l, r <- rest]

combinations4 :: [a] -> [[a]]
combinations4 l = [front ++ [r] |t <- tails l, let front = take 3 t, let rest = drop 3 t, r <- rest]

combinations2 :: [a] -> [[a]]
combinations2 l = [front ++ [r] |t <- tails l, let front = take 1 t, let rest = drop 1 t, r <- rest]

combinations :: [a] -> [[a]]
combinations l = [c |t <- tails l, n <- [1..9], let front = take n t, let rest = drop n t, r <- rest, let c = front ++ [r]]

