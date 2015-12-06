import           AdventUtils

main = openInputAndExecute (print . length . filter isNiceString . lines)

isNiceString :: String -> Bool
isNiceString xs = vowels xs && dups xs && notContains xs

vowels :: String -> Bool
vowels xs = 3 <= length (filter isVowel xs)
  where isVowel x = x `elem` "aeiou"

dups :: String -> Bool
dups = forSomeCharPairs previous
  where
    previous Nothing Nothing = False
    previous (Just _) Nothing = False
    previous (Just x) (Just y) = x == y
    previous _ _ = False

notContains :: String -> Bool
notContains = forEveryCharPair check
  where
    check Nothing Nothing = True
    check (Just _) Nothing = True
    check (Just x) (Just y) = str `notElem` blackList
      where str = [x, y]
            blackList = ["ab", "cd", "pq", "xy"]
    check _ _ = False
