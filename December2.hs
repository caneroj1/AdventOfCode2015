import           AdventUtils
import           Data.List
import           Data.List.Split

main = openInputAndExecute(\contents -> do
  print $ sum $ map fst (boxes contents)
  print $ sum $ map snd (boxes contents))
  where boxes contents = map process (lines contents)

process :: String -> (Int, Int)
process line = (wrappingPaper, ribbons)
  where sizes = map stringsToInts $ removeXs line
        box = getBoxFaces sizes
        wrappingPaper = getAdditionalFace box + sum box
        ribbons = product sizes + doubleAndSum (smallestSide sizes)

stringsToInts :: String -> Int
stringsToInts xs = read xs :: Int

doubleAndSum :: (Int, Int) -> Int
doubleAndSum (x, y) = 2 * x + 2 * y

getBoxFaces :: [Int] -> [Int]
getBoxFaces [l,w,h] = [2*l*w, 2*w*h, 2*h*l]
getBoxFaces _ = []

getAdditionalFace :: [Int] -> Int
getAdditionalFace = (`div` 2) . minimum

removeXs :: String -> [String]
removeXs = splitOn "x"

smallestSide :: [Int] -> (Int, Int)
smallestSide [] = (0, 0)
smallestSide (xs) = (head sides, head $ tail sides)
  where sides = sort xs
