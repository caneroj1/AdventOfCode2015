module AdventUtils
    (
      openInputAndExecute          ,
      conditionForCharPairs        ,
      getCharPairsAndStartPositions,
      Cond(..)                     ,
      PairMap
    ) where

import           Data.Map.Strict    as Map
import           Data.Maybe
import           System.Environment
import           System.IO

data Cond = And | Or deriving (Eq)
type StartPositions = [Int]
type PairMap = Map.Map String StartPositions

openInputAndExecute :: (String -> IO ()) -> IO ()
openInputAndExecute fn = do
  args <- getArgs
  let filePath = head args
  withFile filePath ReadMode (\handle -> do
    contents <- hGetContents handle
    fn contents)

conditionForCharPairs :: (Maybe Char -> Maybe Char -> Bool) -> Cond -> String -> Bool
conditionForCharPairs fn _ [] = fn Nothing Nothing
conditionForCharPairs fn _ [x] = fn (Just x) Nothing
conditionForCharPairs fn cond (x:xs)
  | cond == And = first && second
  | otherwise = first || second
  where nextChar = head xs
        first = fn (Just x) (Just nextChar)
        second = conditionForCharPairs fn cond xs

getCharPairsAndStartPositions :: PairMap -> Int -> String -> PairMap
getCharPairsAndStartPositions m _ [] = m
getCharPairsAndStartPositions m _ [_] = m
getCharPairsAndStartPositions m start (x:xs) =
  getCharPairsAndStartPositions (adjustMap m key start) (start + 1) xs
  where key = [x, head xs]

adjustMap :: PairMap -> String -> Int -> PairMap
adjustMap m k v
  | isNothing item = Map.insert k [v] m
  | otherwise = Map.insert k (fromJust item ++ [v]) m
  where item = Map.lookup k m
