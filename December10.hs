import           Data.List

main = do
  print $ length $ foldl lookAndSay "1113122113" [1..40]
  print $ length $ foldl lookAndSay "1113122113" [1..50]

lookAndSay :: String -> Int -> String
lookAndSay xs _ = concatMap (\item -> show (length item) ++ [head item]) $ group xs
