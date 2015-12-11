import           AdventUtils
import           Data.List
import qualified Data.List.Split as Split
import qualified Data.Map.Lazy   as Map
import           Data.Maybe

type MapItem = (String, Int)

main = openInputAndExecute (\contents -> do
  let inp = lines contents
  let places = clean $ concatMap getPlaceNames inp
  let travelMap = foldl (\acc x -> add acc $ parse x) Map.empty inp
  print "Min: "
  print . minimum . map (toDistance travelMap) $ permutations places

  print "Max: "
  print . maximum . map (toDistance travelMap) $ permutations places)


getPlaceNames :: String -> [String]
getPlaceNames xs = [head names, last names]
  where names = Split.splitOn " to " $ head $ tokens xs

parse :: String -> MapItem
parse xs = (head $ tokens xs, read (last $ tokens xs) :: Int )

tokens :: String -> [String]
tokens = Split.splitOn " = "

clean :: (Eq a, Ord a) => [a] -> [a]
clean = nub . sort

add :: Map.Map String Int -> MapItem -> Map.Map String Int
add m (k, v) = Map.insert k v m

toDistance :: Map.Map String Int -> [String] -> Int
toDistance _ [] = 0
toDistance _ [_] = 0
toDistance m (x:y:xs) = value + toDistance m (y:xs)
  where key = x ++ " to " ++ y
        found = Map.lookup key m
        value = fromMaybe (fromJust $ Map.lookup (y ++ " to " ++ x) m) found
