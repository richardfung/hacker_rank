import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Bits
import Debug.Trace

main = do
    q <- readLn :: IO Int
    forM_ [1..q] $ \_ -> do
        [l,r] <- map read <$> words <$> getLine :: IO [Int]
        print $ solve l r

solve :: Int -> Int -> Int
solve l r = (xor_sum r) `xor` (xor_sum (l-1))

xor_sum :: Int -> Int
xor_sum 0 = 0
xor_sum n =
    let one = if n `mod` 2 == 1 then length `mod` 2
              else 0
        length = n `div` 2 + 1
        pow2 i = shift 1 i
        count i = let chunks = (pow2 $ i-1) * (length `div` (pow2 i))
                      leftovers = max (length
                                       - (pow2 i) * (length `div` (pow2 i))
                                       - (pow2 $ i-1)
                                      ) 0
                  in chunks + leftovers
    in one + (sum $ map (\i -> (pow2 i) * ((count i) `mod` 2))
                       [1..floor $ logBase 2 $ fromIntegral n]
             )
