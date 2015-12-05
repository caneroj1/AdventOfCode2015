import           Crypto.Hash
import qualified Data.ByteString.Char8 as BS
import           Data.List

main = putStrLn . snd . head . filter isGoodHash $ map (md5 secretKey) stringifiedNumbers
  where secretKey = "bgvyzdsv"

stringifiedNumbers :: [String]
stringifiedNumbers = map show [1..]

md5 :: String -> String -> (String, String)
md5 key xs = (show digest, xs)
  where hashable = key ++ xs
        digest = hash (BS.pack hashable) :: Digest MD5

isGoodHash :: (String, String) -> Bool
isGoodHash (digest, xs) = "00000" == take 5 digest
