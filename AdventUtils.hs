module AdventUtils
    (
      openInputAndExecute          ,
      conditionForCharPairs        ,
      getCharPairsAndStartPositions,
      instruction                  ,
      Cond(..)                     ,
      Instruction(..)              ,
      PairMap                      ,
      Rectangle                    ,
      LightGrid                    ,
    ) where

import qualified Data.List.Split    as Split
import qualified Data.Map.Strict    as Map
import           Data.Maybe
import           System.Environment
import           System.IO

-- conditional type for conditionForCharPairs
data Cond = And | Or deriving (Eq)

-- December3
type StartPositions = [Int]

-- December5
type PairMap = Map.Map String StartPositions

-- December6
type CoordinatePair = (Int, Int)
type Rectangle = (CoordinatePair, CoordinatePair)
data Instruction = Toggle Rectangle | Off Rectangle | On Rectangle deriving (Show)
type LightGrid = Map.Map Rectangle Bool

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

instruction :: String -> Instruction
instruction [] = Off ((0,0), (0,0))
instruction xs
  | "turn" == head tokens = makeInstruction $ tail tokens
  | "toggle" == head tokens = makeInstruction tokens
  where tokens = Split.splitOn " " xs
instruction _ = Off ((0,0), (0,0))


makeInstruction :: [String] -> Instruction
makeInstruction tokens
  | first == "on" = On rect
  | first == "off" = Off rect
  | otherwise = Toggle rect
  where first = head tokens
        coord1 = coord $ tokens !! 1
        coord2 = coord $ tokens !! 3
        rect = (coord1, coord2)

coord :: String -> CoordinatePair
coord xs = (read x :: Int, read y :: Int)
  where nums = Split.splitOn "," xs
        x = head nums
        y = last nums
