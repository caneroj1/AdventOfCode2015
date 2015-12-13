import           AdventUtils
import           Data.List
import qualified Data.List.Split as Split
import qualified Data.Map.Strict as Map
import           Data.Maybe

data SeatingArrangement = SeatingArrangement String Int deriving (Show)
type SeatingMap = Map.Map String [SeatingArrangement]

main = openInputAndExecute(\contents -> do
  let people = uniq . sort $ map ((!! 0 ) . Split.splitOn " ") (lines contents)
  let seatingMap = createSeatingArrangements (lines contents) Map.empty
  let newMap = updateSeatingMap seatingMap "me"
  let finalMap = foldl (\acc x -> updateMapList acc "me" x) newMap (makeMyArrangements people)
  let optimum = maximum $ map (happiness finalMap . makeCircular) (permutations ("me" : people))
  print optimum)

updateSeatingMap :: SeatingMap -> String -> SeatingMap
updateSeatingMap m name = foldl (\acc x -> updateMapList acc x (SeatingArrangement name 0)) m (Map.keys m)

makeMyArrangements :: [String] -> [SeatingArrangement]
makeMyArrangements = map (`SeatingArrangement` 0)

happiness :: SeatingMap -> [String] -> Int
happiness m (x:y:z:xs) = accessHappiness y x m + accessHappiness y z m + happiness m (y:z:xs)
happiness _ _ = 0

accessHappiness :: String -> String -> SeatingMap -> Int
accessHappiness k v m = val
  where list = fromJust $ Map.lookup k m
        (SeatingArrangement _ val) = fromJust $ find (\(SeatingArrangement n _) -> n == v) list

createSeatingArrangements :: [String] -> SeatingMap -> SeatingMap
createSeatingArrangements [] m = m
createSeatingArrangements (x:xs) m = createSeatingArrangements xs (updateMapList m name arr)
  where (name, arr) = parse x

updateMapList :: SeatingMap -> String -> SeatingArrangement -> SeatingMap
updateMapList m k a
  | isNothing found = Map.insert k [a] m
  | otherwise = Map.insert k (a : fromJust found) m
  where found = Map.lookup k m

parse :: String -> (String, SeatingArrangement)
parse xs
  | sentiment = (head tokens, SeatingArrangement person val)
  | otherwise = (head tokens, SeatingArrangement person (-val))
  where tokens = Split.splitOn " " xs
        sentiment = (tokens !! 2) == "gain"
        person = init $ last tokens
        val = read (tokens !! 3) :: Int
