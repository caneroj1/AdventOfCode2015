module AdventUtils
    (
      openInputAndExecute           ,
      conditionForCharPairs         ,
      getCharPairsAndStartPositions ,
      openInputAndExecuteByteStrings,
      instruction                   ,
      clength                       ,
      foldl'                        ,
      isNumeric                     ,
      uniq                          ,
      makeCircular                  ,
      makeReindeer                  ,
      Cond(..)                      ,
      Instruction(..)               ,
      Reindeer(..)                  ,
      PairMap                       ,
      Rectangle                     ,
      LightMap
    ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Char             as Char
import qualified Data.List.Split       as Split
import qualified Data.Map.Strict       as Map
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
data Instruction = Toggle Rectangle | Off Rectangle | On Rectangle deriving (Show, Eq)
type LightMap = Map.Map Int Int

-- December14
data Reindeer = Reindeer String Int Int Int Int Int Int Int | Finished String Int

instance Eq Reindeer where
  (Finished xs x) == (Finished ys y) = xs == ys && x == y
  (Reindeer _ _ _ _ pX _ _ _) == (Reindeer _ _ _ _ pY _ _ _) = pX == pY

instance Ord Reindeer where
  (Finished _ x) <= (Finished _ y) = x <= y
  (Reindeer _ _ _ _ pX _ _ _) <= (Reindeer _ _ _ _ pY _ _ _) = pX <= pY

instance Show Reindeer where
  show (Finished name x) = name ++ " finished after " ++ show x ++ " km."
  show (Reindeer name _ _ _ points _ _ _) = name ++ " finished with " ++ show points ++ " points."

openInputAndExecute :: (String -> IO ()) -> IO ()
openInputAndExecute fn = do
  args <- getArgs
  let filePath = head args
  withFile filePath ReadMode (\handle -> do
    contents <- hGetContents handle
    fn contents)

openInputAndExecuteByteStrings :: (BS.ByteString -> IO ()) -> IO ()
openInputAndExecuteByteStrings fn = do
  args <- getArgs
  let filePath = head args
  withFile filePath ReadMode (\handle -> do
    contents <- BS.hGetContents handle
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

clength :: [a] -> Int
clength xs = length' xs 0
  where length' []     n = n
        length' (_:ys) n = length' ys $! (n + 1)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ z []     = z
foldl' f z (x:xs) = let z' = z `f` x
                    in seq z' $ foldl' f z' xs

isNumeric :: String -> Bool
isNumeric [] = False
isNumeric [x] = Char.isNumber x || x == '-'
isNumeric (x:xs) = (Char.isNumber x || x == '-') && isNumeric xs

uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq [x] = [x]
uniq (x:xs) = if x == head xs then uniq xs else x : uniq xs

makeCircular :: [a] -> [a]
makeCircular xs = circle xs
  where l = clength xs
        circle = take (l+2) . drop (l-1) . take (l*3) . cycle

makeReindeer :: String -> Reindeer
makeReindeer xs = Reindeer name speed time rest 0 0 time rest
  where tokens = Split.splitOn " " xs
        name = head tokens
        speed = read (tokens !! 3) :: Int
        time = read (tokens !! 6) :: Int
        rest = read (tokens !! 13) :: Int
