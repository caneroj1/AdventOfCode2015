import           AdventUtils
import qualified Data.Map.Strict as Map
import           Data.Maybe

type Key = (Int, Int)
type Positions = (Key, Key)
type IntMap = Map.Map Key Int

main = openInputAndExecute (print . process)

process :: String -> Int
process [] = 0
process xs = Map.size $ parseInput xs startMap startPositions True
  where startMap = Map.fromList [((0,0), 1)]
        startPositions = ( (0,0), (0,0) )

parseInput :: String -> IntMap -> Positions -> Bool -> IntMap
parseInput [] m _ _ = m
parseInput (x:xs) m ((si, sj), (ri, rj)) who = parseInput xs newMap newPositions (not who)
  where
    newMap = if who then adjustMap m (si, sj) 1 else adjustMap m (ri, rj) 1
    newPositions = getNewPositions x who ((si, sj), (ri, rj))

getNewPositions :: Char -> Bool -> Positions -> Positions
getNewPositions x who ((si, sj), (ri, rj))
  | x == '^' = if who then ((si, sj+1), (ri, rj)) else ((si, sj), (ri, rj+1))
  | x == '>' = if who then ((si+1, sj), (ri, rj)) else ((si, sj), (ri+1, rj))
  | x == 'v' = if who then ((si, sj-1), (ri, rj)) else ((si, sj), (ri, rj-1))
  | x == '<' = if who then ((si-1, sj), (ri, rj)) else ((si, sj), (ri-1, rj))
  | otherwise = ((si, sj), (ri, rj))


adjustMap :: IntMap -> Key -> Int -> IntMap
adjustMap m k v
  | isNothing item = Map.insert k v m
  | otherwise = Map.insert k (v + fromJust item) m
  where item = Map.lookup k m
