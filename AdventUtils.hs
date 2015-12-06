module AdventUtils
    (
      openInputAndExecute ,
      forEveryCharPair    ,
      forSomeCharPairs
    ) where

import           System.Environment
import           System.IO

openInputAndExecute :: (String -> IO ()) -> IO ()
openInputAndExecute fn = do
  args <- getArgs
  let filePath = head args
  withFile filePath ReadMode (\handle -> do
    contents <- hGetContents handle
    fn contents)

forEveryCharPair :: (Maybe Char -> Maybe Char -> Bool) -> String -> Bool
forEveryCharPair fn [] = fn Nothing Nothing
forEveryCharPair fn [x] = fn (Just x) Nothing
forEveryCharPair fn (x:xs) = fn (Just x) (Just nextChar) && forEveryCharPair fn xs
  where nextChar = head xs

forSomeCharPairs :: (Maybe Char -> Maybe Char -> Bool) -> String -> Bool
forSomeCharPairs fn [] = fn Nothing Nothing
forSomeCharPairs fn [x] = fn (Just x) Nothing
forSomeCharPairs fn (x:xs) = fn (Just x) (Just nextChar) || forSomeCharPairs fn xs
  where nextChar = head xs
