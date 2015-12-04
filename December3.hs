import           AdventUtils
import qualified Data.Map.Strict as Map
import           Data.Maybe

type Key = (Int, Int)
type IntMap = Map.Map Key Int

main = openInputAndExecute(\contents -> do
  print $ process contents)

process :: String -> Int
process [] = 0
process xs = Map.size $ parseInput xs startMap (0,0)
  where startMap = Map.fromList [((0,0), 1)]

parseInput :: String -> IntMap -> Key -> IntMap
parseInput [] m _ = m
parseInput (x:xs) m (i, j)
  | x == '^' = parseInput xs newMap (i, j+1)
  | x == '>' = parseInput xs newMap (i+1, j)
  | x == 'v' = parseInput xs newMap (i, j-1)
  | x == '<' = parseInput xs newMap (i-1, j)
  where newMap = adjustMap m (i,j) 1

adjustMap :: IntMap -> Key -> Int -> IntMap
adjustMap m k v
  | isNothing item = Map.insert k v m
  | otherwise = Map.insert k (v + fromJust item) m
  where item = Map.lookup k m
