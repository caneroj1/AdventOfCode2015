import           AdventUtils
import qualified Data.Map.Strict as Map
import           Data.Maybe

main = openInputAndExecute (\contents -> do
  let grid = Map.empty
  let finishedMap = foldl' applyInstruction grid . map instruction $ lines contents
  let list = Map.toList finishedMap
  print $ foldl' (+) 0 $ map snd list)

applyInstruction :: LightMap -> Instruction -> LightMap
applyInstruction mp (Off     rect ) = foldl' (\m idx -> setMap m idx (-1)) mp $ indexes rect
applyInstruction mp (On      rect ) = foldl' (\m idx -> setMap m idx 1) mp $ indexes rect
applyInstruction mp (Toggle  rect ) = foldl' (\m idx -> setMap m idx 2) mp $ indexes rect

-- we have a 1000x1000 grid. 0-999, 0-999
indexes :: Rectangle -> [Int]
indexes ((xi, yi), (xj, yj))
  | yj > yi = [1000*yj + xi..1000*yj + xj] ++ indexes ((xi, yi), (xj, yj-1))
  | yj == yi = [1000*yj + xi..1000*yj + xj]
indexes _ = []

setMap :: LightMap -> Int -> Int -> LightMap
setMap m index v
  | isNothing item = Map.insertWith f index (max 0 v) m
  | otherwise = Map.insertWith f index v m
  where f n o = if n + o < 0 then 0 else n + o
        item = Map.lookup index m
