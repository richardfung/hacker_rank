import Control.Applicative ((<$>))
import Control.Monad (mapM, mapM_)
import Data.List (sort, tails)
import Data.Map.Strict as M

main = do
    t <- readLn :: IO Int
    ss <- mapM (\_ -> getLine) [1..t]
    mapM_ putStrLn $! Prelude.map (show . count . getAnagramSets) ss

count :: Map String Int -> Int
count m =
    sum $! Prelude.map (\i -> (i * (i-1)) `div` 2) $! elems m

getAnagramSets :: String -> Map String Int
getAnagramSets s =
    Prelude.foldr (\temp -> getSet (reverse temp)) M.empty $! tails s
    where getSet :: String -> Map String Int -> Map String Int
          getSet [] m = m
          getSet s@(x:xs) m =
            let current = findWithDefault 0 key m
                key = sort s
            in getSet xs $! insert key (current+1) m
