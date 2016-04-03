import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Char (digitToInt)

main = do
    [_, q] <- map read <$> words <$> getLine :: IO [Int]
    digits <- map digitToInt <$> getLine :: IO [Int]
    let zeroes = length $! filter (0 ==) digits
        l = length digits
        lengths = map (\i -> subseqLength i l zeroes) digits
    forM_ [1..q] $ \_ -> do
        index <- readLn :: IO Int
        print digits

subseqLength :: Int -> Int -> Int -> Int
subseqLength 1 len zeroes = len
subseqLength i len zeroes = len - i + 1 - zeroes

accLengths :: [Int] -> [Int]
accLengths xs =
    let helper :: [Int] -> Int -> [Int]
        helper [] _ = []
        helper (x:xs) acc = (id $! acc+x):(helper xs acc)
    in helper xs 0
