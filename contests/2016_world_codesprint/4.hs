import Control.Applicative ((<$>))
import Debug.Trace

modConst = 10^9 + 7

main = do
    [rr, rb, bb, br] <- map read <$>  words <$> getLine :: IO [Int]
    print $ count rr rb bb br

count :: Int -> Int -> Int -> Int -> Int
count rr rb bb br
    | rb == br = (
          ((helper rr $ rb+1) * (helper bb $ rb))
           + ((helper rr $ rb) * (helper bb $ rb+1))
                 ) `mod` modConst
    | abs (rb - br) == 1 = ((helper rr $ max rb br) * (helper bb $ max rb br)) `mod` modConst
    | otherwise = 0
    where helper :: Int -> Int -> Int
          helper xx x = (choose (toInteger $ xx + x - 1) (toInteger xx)) `mod` modConst

choose :: Integer -> Integer -> Int
choose n 0 = 1
choose 0 k = 0
choose n k = let helper :: Integer -> Integer -> Integer
                 helper n 0 = 1
                 helper 0 k = 0
                 helper n k = helper (n-1) (k-1) * n `div` k
             in fromInteger $ (helper n k) `mod` (toInteger modConst)
