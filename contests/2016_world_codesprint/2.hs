import Control.Applicative ((<$>))
import Data.List

main = do
    [n,m] <- map read <$> words <$> getLine :: IO [Int]
    cs <- sort <$> map read <$> words <$> getLine :: IO [Int]
    print $ maxDistance n cs

maxDistance :: Int -> [Int] -> Int
maxDistance n cs =
    let leftmost = head cs
        rightmost = n - 1 - (maximum cs)
        findMiddles [a] = []
        findMiddles (a:b:cs) = ((b-a) `div` 2) : (findMiddles $ b:cs)
    in maximum $ leftmost:rightmost:(findMiddles cs)
