import           AdventUtils
import qualified Data.List.Split as Split

main = openInputAndExecute (print . maximum . map (processReindeer 2503 . makeReindeer) . lines)

processReindeer :: Int -> Reindeer -> Reindeer
processReindeer total (Reindeer name speed time rest _ _ _ _) =
  Finished name (getDistance total speed time rest)

getDistance :: Int -> Int -> Int -> Int -> Int
getDistance totalTime distance flyTime restTime = result
  where reindeerTime = flyTime + restTime
        (cycles, rest) = (totalTime `div` reindeerTime, totalTime `rem` reindeerTime)
        final = cycles * (distance * flyTime)
        result = if rest == 0
                then final
                else
                  case compare rest flyTime of
                    LT -> final + rest `rem` flyTime
                    _ -> final + (distance * flyTime)
