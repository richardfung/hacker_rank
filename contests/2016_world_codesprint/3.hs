import Control.Applicative ((<$>), (<*>))
import Control.Monad (mapM)
import Data.Array.Unboxed
import Data.List (maximumBy)
import Debug.Trace

-- (r,c,length of segments not including middle)
type Cross = (Int,Int,Int)

main = do
    [n,m] <- map read <$> words <$> getLine :: IO [Int]
    m <- listArray ((0,0), (n-1,m-1)) <$> concat <$> mapM (\_ -> getLine) [1..n]
            :: IO (UArray (Int,Int) Char)
    print $ solve m

-- find max cross at each index
-- for each index, get max of other crosses that don't intersect
-- get max of all maxes

solve :: UArray (Int,Int) Char -> Int
solve m =
    let goodPs = filter (\(r,c) -> good m r c) $ indices m
        crosses = concat $ map (\(r,c) -> getCrosses m r c) $ goodPs
        pairs = map (\a -> \b -> (a,b)) crosses <*> crosses
        prods = map (\(a,b) -> if overlaps a b then 0 else (area a) * (area b))
            pairs
    in maximum prods

area :: Cross -> Int
area (_,_,l) = 1 + 4*l

getCrosses :: UArray (Int,Int) Char -> Int -> Int -> [Cross]
getCrosses m r c =
    let helper n = if and $ map (\(r, c) -> good m r c)
                          [(r-n,c), (r+n,c), (r,c-n), (r,c+n)] then helper $ n+1
                   else n-1
        maxLength = helper 1
    in map (\l -> (r,c,l)) [0..maxLength]

good :: UArray (Int,Int) Char -> Int -> Int -> Bool
good m r c = inRange (bounds m) (r,c) && (m ! (r,c) == 'G')

overlaps :: Cross -> Cross -> Bool
overlaps (r0,c0,l0) (r1,c1,l1)
    | r0 == r1 = abs (r1 - r0) <= l0 + l1
    | c0 == c1 = abs (c1 - c0) <= l0 + l1
    | otherwise = let dr = abs $! r1-r0
                      dc = abs $! c1-c0
                  in (min dr dc <= min l0 l1) && (max dr dc <= max l0 l1)
