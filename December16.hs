import           AdventUtils
import           Control.Applicative
import           Data.List
import qualified Data.List.Split     as Split

data Aunt = Aunt {
  children    :: Maybe Int,
  cats        :: Maybe Int,
  samoyeds    :: Maybe Int,
  pomeranians :: Maybe Int,
  akitas      :: Maybe Int,
  vizslas     :: Maybe Int,
  goldfish    :: Maybe Int,
  trees       :: Maybe Int,
  cars        :: Maybe Int,
  perfumes    :: Maybe Int,
  id          :: Int
} deriving (Show)

comps = [ children, cats, samoyeds, pomeranians,
          akitas, vizslas, goldfish, trees, cars, perfumes ]

comps' = [(==), (<), (==), (>),
          (==), (==), (>), (<), (==), (==) ]

auntCompare :: Maybe Int -> Maybe Int -> Bool
auntCompare (Just x) (Just y) = x == y
auntCompare _ _ = True

auntCompare' :: (Int -> Int -> Bool, Maybe Int) -> Maybe Int -> Bool
auntCompare' (f, Just x) (Just y) = f x y
auntCompare' _ _ = True

compareAunts :: Aunt -> Aunt -> Bool
compareAunts a b = and $ zipWith auntCompare' newComp bStuff
  where aStuff = map (\func -> func a) comps
        newComp = zipWith (\func val -> (func,val)) comps' aStuff
        bStuff = map (\func -> func b) comps

parse :: String -> String
parse xs = if ',' `elem` xs then init xs else xs

tokMap :: [String] -> Int -> Int
tokMap tokens idx = read $ parse (tokens !! (idx + 1)) :: Int

toAunt :: String -> Aunt
toAunt xs = Aunt children cats samoyeds pomeranians akitas vizslas goldfish trees cars perfumes aId
  where tokens = Split.splitOn " " xs
        children    = tokMap tokens <$> elemIndex "children:" tokens
        cats        = tokMap tokens <$> elemIndex "cats:" tokens
        samoyeds    = tokMap tokens <$> elemIndex "samoyeds:" tokens
        pomeranians = tokMap tokens <$> elemIndex "pomeranians:" tokens
        akitas      = tokMap tokens <$> elemIndex "akitas:" tokens
        vizslas     = tokMap tokens <$> elemIndex "vizslas:" tokens
        goldfish    = tokMap tokens <$> elemIndex "goldfish:" tokens
        trees       = tokMap tokens <$> elemIndex "trees:" tokens
        cars        = tokMap tokens <$> elemIndex "cars:" tokens
        perfumes    = tokMap tokens <$> elemIndex "perfumes:" tokens
        aId         = read (parse $ init (tokens !! 1)) :: Int

trueAunt = Aunt (Just 3) (Just 7) (Just 2) (Just 3) (Just 0) (Just 0) (Just 5) (Just 3) (Just 2) (Just 1) 0

main = openInputAndExecute (\content -> do
  let aunts = map toAunt $ lines content
  let goodAunts = filter (compareAunts trueAunt) aunts
  print $ head goodAunts)
