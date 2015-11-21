import Control.Applicative ((<$>))
import Control.Monad (forM_, when)
import Data.Array.ST
import Data.Array.Unboxed

main = do
    n <- readLn :: IO Int
    values  <- listArray (1, n) <$> (map read) <$> words <$> getLine
        :: IO (Array Int Int)
    print $ solve n values

solve :: Int -> Array Int Int -> Int
solve n values = sum $ map f [1..n]
    where pList = genPrimes n
          pArray = listArray (1, length pList) pList :: Array Int Int
          f i = (values ! i) * ((count (i-1) pArray) - (count (n-i) pArray) )

{- finds the number of elements <= n -}
count :: Int -> Array Int Int -> Int
count n xs = if i < (fst $ bounds xs) then 0
             else i - (fst $ bounds xs) + 1
    where
          -- return index of biggest element <= n
          binSearch :: Int -> Int -> Int
          binSearch s e = if s == e then if xs ! s <= n then s
                                         else s-1
                          else let m = (s+e) `div` 2
                               in if xs ! m <= n then binSearch (m+1) e
                                  else binSearch s m
          i = binSearch (fst $ bounds xs) (snd $ bounds xs)

genPrimes :: Int -> [Int]
genPrimes n = map fst $ filter (\x -> True == snd x) $ assocs $ runSTUArray $ do
    ret <- newArray (2,n) True
    forM_ [2..ceiling . sqrt $ fromIntegral n] $ \i -> do
        isPrime <- readArray ret i
        when isPrime $ do
            forM_ [2*i, 3*i..n] $ \j -> do
                writeArray ret j False
    return ret
