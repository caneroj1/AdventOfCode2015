module AdventUtils
    (
      openInputAndExecute
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
