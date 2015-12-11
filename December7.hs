import           AdventUtils
import qualified Data.Bits       as Bits
import qualified Data.List.Split as Split
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Word
import           Debug.Trace

main = openInputAndExecute (\contents -> do
  let originalMap = foldl toCircuit Map.empty $ lines contents
  let aValue = findValue "a" originalMap
  let newValue = findValue "a" $ set originalMap "b" (snd aValue)
  print $ snd newValue )

data Circuit =  RSHIFT String Int |
                LSHIFT String Int |
                AND String String |
                OR String String  |
                NOT String        |
                WIRE String       |
                VALUE Word16 deriving (Show)

type CircuitMap = Map.Map String Circuit

findValue :: String -> CircuitMap -> (CircuitMap, Word16)
findValue xs m = (newMap, snd pair)
  where val = fromJust $ Map.lookup xs m
        pair = eval m val
        newMap = set (fst pair) xs (snd pair)

eval :: CircuitMap -> Circuit -> (CircuitMap, Word16)
eval m (RSHIFT xs n) = (newMap, Bits.shift (snd pair1) (-n))
  where nextCircuit = get m xs
        pair1 = eval m nextCircuit
        newMap = set (fst pair1) xs (snd pair1)
eval m (LSHIFT xs n) = (newMap, Bits.shift (snd pair1) n)
  where nextCircuit = get m xs
        pair1 = eval m nextCircuit
        newMap = set (fst pair1) xs (snd pair1)
eval m (AND xs ys) = case isNumeric xs of
  True -> (fst otherPair1, (read xs :: Word16) Bits..&. snd otherPair1)
  False -> (finalMap, snd pair1 Bits..&. snd pair2)
  where nextCircuit = get m xs
        nextCircuit2 = get m ys
        pair1 = eval m nextCircuit
        otherPair1 = eval m nextCircuit2
        newMap = set (fst pair1) xs (snd pair1)
        pair2 = eval newMap nextCircuit2
        finalMap = set (fst pair2) ys (snd pair2)
eval m (OR xs ys) = (finalMap, snd pair1 Bits..|. snd pair2)
  where nextCircuit = get m xs
        nextCircuit2 = get m ys
        pair1 = eval m nextCircuit
        newMap = set (fst pair1) xs (snd pair1)
        pair2 = eval newMap nextCircuit2
        finalMap = set (fst pair2) ys (snd pair2)
eval m (NOT xs) = (newMap, val)
  where nextCircuit = get m xs
        pair = eval m nextCircuit
        newMap = set m xs (snd pair)
        val = Bits.complement (snd pair)
eval m (WIRE xs) = (newMap, snd pair)
  where nextCircuit = get m xs
        pair = eval m nextCircuit
        newMap = set (fst pair) xs (snd pair)
eval m (VALUE x) = (m, x)

set :: CircuitMap -> String -> Word16 -> CircuitMap
set m k w = Map.insert k (VALUE w) m

valueToWord :: Circuit -> Word16
valueToWord (VALUE x) = x

get :: CircuitMap -> String -> Circuit
get m xs = fromJust $ Map.lookup xs m

toCircuit :: CircuitMap -> String -> CircuitMap
toCircuit m xs
  | length tokens == 3 = let v = head tokens in
                         if isNumeric v then
                           Map.insert label (VALUE (read v :: Word16)) m
                         else
                           Map.insert label (WIRE v) m
  | length tokens == 4 = let v = tokens !! 1 in Map.insert label (NOT v) m
  | length tokens == 5 = let op = tokens !! 1 in
                         case op of
                           "RSHIFT" -> let shift = (read (tokens !! 2) :: Int) in Map.insert label (RSHIFT (head tokens) shift) m
                           "LSHIFT" -> let shift = (read (tokens !! 2) :: Int) in Map.insert label (LSHIFT (head tokens) shift) m
                           "AND" -> Map.insert label (AND (head tokens) (tokens !! 2)) m
                           "OR" -> Map.insert label (OR (head tokens) (tokens !! 2)) m
                           _ -> Map.empty
  where tokens = Split.splitOn " " xs
        label = last tokens
