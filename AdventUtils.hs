module AdventUtils
    (
      openInputAndExecute      ,
      conditionForCharPairs    ,
      Cond(..)
    ) where

import           System.Environment
import           System.IO

data Cond = And | Or deriving (Eq)

openInputAndExecute :: (String -> IO ()) -> IO ()
openInputAndExecute fn = do
  args <- getArgs
  let filePath = head args
  withFile filePath ReadMode (\handle -> do
    contents <- hGetContents handle
    fn contents)

conditionForCharPairs :: (Maybe Char -> Maybe Char -> Bool) -> Cond -> String -> Bool
conditionForCharPairs fn _ [] = fn Nothing Nothing
conditionForCharPairs fn _ [x] = fn (Just x) Nothing
conditionForCharPairs fn cond (x:xs)
  | cond == And = first && second
  | otherwise = first || second
  where nextChar = head xs
        first = fn (Just x) (Just nextChar)
        second = conditionForCharPairs fn cond xs
