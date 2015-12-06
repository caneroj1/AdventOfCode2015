import           AdventUtils
import qualified Data.Map.Strict as Map
import           Data.Maybe

main = openInputAndExecute (print . length . filter isNaughtyOrNice . lines)

isNaughtyOrNice :: String -> Bool
isNaughtyOrNice xs = disjointPairs pairMap && letterCheck xs
  where pairMap = getCharPairsAndStartPositions Map.empty 0 xs

disjointPairs :: PairMap -> Bool
disjointPairs m = any noOverlap values
  where list = Map.toList m
        values = map snd list

noOverlap :: [Int] -> Bool
noOverlap [] = False
noOverlap [_] = False
noOverlap xs = (last xs - head xs) > 1
  where disjoint x y = y - x > 1


letterCheck :: String -> Bool
letterCheck [_, _] = False
letterCheck (x:y:xs) = (x == head xs) || letterCheck (y:xs)
