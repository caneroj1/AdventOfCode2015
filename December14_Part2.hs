import           AdventUtils
import qualified Data.List.Split as Split

main = openInputAndExecute (\contents -> do
  let reindeers = map makeReindeer (lines contents)
  let res = foldl' race reindeers [1..2503]
  print $ maximum res)

race :: [Reindeer] -> Int -> [Reindeer]
race reindeers _ = newRound
  where advancedDeers = map advanceReindeer reindeers
        maxDist = maximum $ map (\(Reindeer _ _ _ _ _ traveled _ _) -> traveled) advancedDeers
        winningDeers = filter (\(Reindeer _ _ _ _ _ traveled _ _) -> traveled == maxDist) advancedDeers
        otherDeers = filter (\(Reindeer _ _ _ _ _ traveled _ _) -> traveled < maxDist) advancedDeers
        newRound = otherDeers ++ map (\(Reindeer a b c d points e f g) -> (Reindeer a b c d (points+1) e f g)) winningDeers

advanceReindeer :: Reindeer -> Reindeer
advanceReindeer (Reindeer name speed 0    0    points traveled origTime origRest) = Reindeer name speed (origTime-1) origRest points (traveled+speed) origTime origRest
advanceReindeer (Reindeer name speed 0    rest points traveled origTime origRest) = Reindeer name speed 0 (rest-1) points traveled origTime origRest
advanceReindeer (Reindeer name speed time rest points traveled origTime origRest) = Reindeer name speed (time-1) rest points (traveled+speed) origTime origRest
