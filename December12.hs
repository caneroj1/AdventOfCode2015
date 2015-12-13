import           AdventUtils
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict        as Map
import           Data.Maybe
import           Data.Scientific
import qualified Data.Text                  as T

type Key = (T.Text, Value)

main = openInputAndExecute(\contents -> do
  let jsonData = decode (BS.pack contents) :: Maybe Object
  let results = concatMap traverseJSON (canTraverse $ Map.toList $ fromJust jsonData)
  print $ sum results)

traverseJSON :: Key -> [Integer]
traverseJSON (_, Object obj) = Prelude.concatMap traverseJSON (canTraverse $ Map.toList obj)
traverseJSON (_, Array  arr) = Prelude.concatMap (traverseJSON . (\x -> (T.pack "", x))) arr
traverseJSON (_, Number num) = [coefficient num]
traverseJSON (_, String str) = [read unpacked :: Integer | isNumeric unpacked]
  where unpacked = T.unpack str
traverseJSON _ = []

canTraverse :: [Key] -> [Key]
canTraverse keys = if all isNotRedProperty $ map snd keys then keys else []
  where isNotRedProperty (String str) = T.pack "red" /= str
        isNotRedProperty _ = True
