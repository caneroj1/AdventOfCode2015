import           AdventUtils
import qualified Data.ByteString.Char8 as BS
import qualified Foreign.Ptr           as P
import           System.IO

-- main = openInputAndExecuteByteStrings (\bytestring -> do
--   let a = BS.head bytestring
--   print a
--   print bytestring)

main = do
  let path = "/Users/jcanero/Code/Repositories/haskell/advent_calendar/December8_Input/input.txt"
  withBinaryFile path ReadMode (\handle -> do
    print "this"
    let ptr = P.nullPtr
    d <- hGetBuf handle ptr 1
    print d)
  print "hello"

inMemory :: String -> Int
inMemory [] = 0
inMemory (x:xs)
  | x == '\\' = 1 + inMemory (rest xs)
  | otherwise = 1 + inMemory xs
  where
    rest [] = []
    rest (y:ys)
      | y == '\\' || y == '"' = ys
      | y == 'x' = tail $ tail ys
      | otherwise = ys
