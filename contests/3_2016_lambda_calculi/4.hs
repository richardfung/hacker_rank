import Control.Applicative ((<$>))
import Control.Monad (mapM)
import Data.List (find, sort)
import Data.Maybe
import Debug.Trace

main = do
  n <- readLn :: IO Int
  points <- orderPoints <$> mapM (\_ -> (\[a,b] -> (read a, read b))
                                  <$> words <$> getLine
                                 ) [1..n] :: IO [(Int,Int)]
  putStrLn $ if isConcave points then "YES" else "NO"

averageHeight :: [(Int, Int)] -> Double
averageHeight xs =
  let hs = map (fromIntegral . snd) xs :: [Double]
  in (sum hs) / (fromIntegral $ length hs)

orderPoints :: [(Int, Int)] -> [(Int, Int)]
orderPoints points =
  let midHeight = averageHeight points
      topHalf = sort $ filter (\p -> (fromIntegral $ snd p) >= midHeight) points
      botHalf = reverse $ sort $ filter (\p -> (fromIntegral $ snd p) < midHeight) points
  in topHalf ++ botHalf

isConcave :: [(Int,Int)] -> Bool
isConcave points =
  let repeated = concat $ repeat points
      zip3 [] _ _ = []
      zip3 _ [] _ = []
      zip3 _ _ [] = []
      zip3 (a:as) (b:bs) (c:cs) = (a,b,c):(zip3 as bs cs)
      triplets = zip3 points (tail repeated) (drop 2 repeated)
      zCrossProduct ((x0,y0), (x1,y1), (x2,y2)) =
        (x1-x0)*(y2-y1) - (y1-y0)*(x2-x1)
      zs = map zCrossProduct triplets
  in not $ (all (>= 0) zs) || (all (<= 0) zs)
