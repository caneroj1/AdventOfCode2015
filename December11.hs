import           AdventUtils
import qualified Data.Map.Lazy as Map

main = do
  let firstNew = getNextPassword "hepxcrrq"
  print ("First New Password: " ++ firstNew)
  let nextNew = getNextPassword firstNew
  print ("Final Password: " ++ nextNew)

getNextPassword :: String -> String
getNextPassword xs = if found then next else getNextPassword next
  where next = increment xs
        found = letterCheck next && hasStraight next && pairCheck next

letterCheck :: String -> Bool
letterCheck = empty . filter (`elem` "il")

empty :: [a] -> Bool
empty [] = True
empty _  = False

isSame :: (Eq a) => [a] -> Bool
isSame [] = True
isSame xs = length xs == length (takeWhile (== head xs) xs)

hasStraight :: String -> Bool
hasStraight [] = False
hasStraight (x:xs) =
  if length ys >= 2
  then head ys == succ x && (ys !! 1) == succ (succ x) || hasStraight xs
  else hasStraight xs
  where (ys, _) = span (>x) xs

pairCheck :: String -> Bool
pairCheck xs =
  case length equalPairs of
  0 -> False
  1 -> checkTuple (head equalPairs)
  _ -> True
  where pairMap = getCharPairsAndStartPositions Map.empty 0 xs
        equalPairs = filter (isSame . fst) $ Map.toList pairMap

checkTuple :: (String, [Int]) -> Bool
checkTuple (_, [_]) = False
checkTuple (_, x:xs) = last xs - x >= 2

increment :: String -> String
increment [] = []
increment xs =
  if shouldWrap
  then increment (init xs) ++ [newLast]
  else init xs ++ [newLast]
  where back = last xs
        newLast = if succ back == '{' then 'a' else succ back
        shouldWrap = newLast == 'a'
