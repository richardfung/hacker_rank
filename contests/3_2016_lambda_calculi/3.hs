import Control.Applicative ((<$>))

main = do
  n <- readLn :: IO Int
  points <- mapM (\_ -> (\[a,b] -> (read a, read b)) <$> words <$> getLine)
            [1..n] :: IO [(Int, Int)]
  putStrLn $ show $ area points

area :: [(Int, Int)] -> Double
area points =
  let pairs = zip points $ tail $ concat $ repeat points
      areas = map (\((x0,y0), (x1, y1)) -> (x0 + x1) * (y0 - y1)) pairs
  in abs $ (fromIntegral $ sum areas) / 2
