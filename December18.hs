import           AdventUtils
import qualified Data.Vector as V

type IndexPair = (Int, Int)
type Lights = V.Vector Int

bound = 100

indices :: V.Vector IndexPair
indices = V.fromList [ (x, y) | y <- [0..bound - 1], x <- [0..bound - 1] ]

neighbors :: IndexPair -> [IndexPair]
neighbors (x,y) = filter bounds [(x-1, y-1), (x, y-1  ), (x+1, y-1),
                   (x-1, y  ), (x+1, y  ), (x-1, y+1),
                   (x, y+1  ), (x+1, y+1)]
  where bounds (i, j) = i >= 0 && i <= bound - 1 && j >= 0 && j <= bound - 1

toPosition :: IndexPair -> Int
toPosition (x, y) = x + (y * bound)

at :: Lights -> IndexPair -> Int
at grid pair = grid V.! toPosition pair

update :: Lights -> IndexPair -> Int
update lights (x, y)
  | toPosition (x, y) `elem` corners = 1
  | status == 1 = if count == 2 || count == 3 then 1 else 0
  | otherwise = if count == 3 then 1 else 0
  where status = at lights (x,y)
        count = sum $ map (at lights) (neighbors (x,y))

corners :: [Int]
corners = [toPosition (0, bound - 1),
           toPosition (bound - 1, bound - 1),
           toPosition (0, 0),
           toPosition (bound - 1, 0)]

toLights :: [String] -> Lights
toLights lns = V.fromList $ lns >>= map (\c -> if c == '#' then 1 :: Int else 0)

fixLights :: Lights -> Lights
fixLights lights = lights V.// map (\i -> (i, 1)) corners

main = openInputAndExecute (\content -> do
  let lights = fixLights . toLights $ lines content
  let iter = V.replicate bound True
  let final = V.foldl' (\l _ -> V.map (update l) indices) lights iter
  print . V.length $ V.filter (==1) final)
