main = do
  n <- readLn :: IO Int
  c <- getLine
  putStrLn $ show $ getChanges c 'X'

getChanges :: String -> Char -> Int
getChanges [] _ = 0
getChanges (x:xs) c = if x == c then getChanges xs c
                      else getChanges xs x + 1
