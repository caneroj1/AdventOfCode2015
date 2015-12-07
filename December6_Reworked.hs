import           AdventUtils
import qualified Data.Map.Strict     as Map
import           Data.Maybe
import qualified Data.Vector.Unboxed as U

type LightVector = U.Vector Int

main = openInputAndExecute (\contents -> do
  let grid = U.replicate 1000000 0
  let finished = foldl' applyInstruction grid . map instruction $ lines contents
  print $ U.sum finished)

applyInstruction :: LightVector -> Instruction -> LightVector
applyInstruction v (Off     rect ) = U.update v $ proper (-1) v $ indexes rect
applyInstruction v (On      rect ) = U.update v $ proper 1 v $ indexes rect
applyInstruction v (Toggle  rect ) = U.update v $ proper 2 v $ indexes rect

-- we have a 1000x1000 grid. 0-999, 0-999
indexes :: Rectangle -> U.Vector Int
indexes ((xi, yi), (xj, yj))
  | yj > yi = make (1000*yj + xi) (1000*yj + xj) U.++ indexes ((xi, yi), (xj, yj-1))
  | yj == yi = make (1000*yj + xi) (1000*yj + xj)
  where make begin end = U.generate (end - begin + 1) (\idx -> begin + idx)
indexes _ = U.empty

proper :: Int -> U.Vector Int -> U.Vector Int -> U.Vector (Int, Int)
proper update oldv = U.map (\x -> (x, change (oldv U.! x) update))
  where change new old = if new + old < 0 then 0 else new + old
