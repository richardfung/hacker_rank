import Control.Monad (forM_)

main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    s <- getLine
    putStrLn $ answer s

answer :: String -> String
answer s =
  let ss = differences s
      rs = differences $ reverse s
      combined = zip ss rs
  in if and $ map (\(a,b) -> a == b) combined then "Funny"
     else "Not Funny"

differences :: String -> [Int]
differences [] = []
differences [_] = []
differences (c:cs) =
  (abs ((fromEnum $ head cs) - (fromEnum c))):(differences cs)
