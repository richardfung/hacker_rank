import Data.List (elemIndex, sort)
import Data.Map.Strict as M
import Data.Maybe
import Debug.Trace

type Count = Map Char Int

main = do
    s <- getLine
    putStrLn $! answer s

add :: (Ord k) => k -> Map k Int -> Map k Int
add k v m = insert k (get k m + v)  m

get :: (Ord k) => k -> Map k Int -> Int
get k m = findWithDefault 0 k m

decrement :: (Ord k) => k -> Map k Int -> Map k Int
decrement k m = add k (-1) m

increment :: (Ord k) => k -> Map k Int -> Map k Int
increment k m = add k 1 m

answer :: String -> String
answer s = helper (reverse s) "" M.empty totals []
    where helper :: String -> String -> Count -> [String] -> Count -> Count
                    -> Count
          helper (c:cs) temp tempCounts revAcc accCounts shuffleCounts
                 totalCounts
              | (get c tempCounts) + (get c shuffleCounts) < totalCounts =
                    helper cs (c:temp) (increment c tempCounts) revAcc accCounts
                           shuffleCounts totalCounts
              | otherwise =
                    let sortedTemp = sort temp
                        cPos = fromJust $! elemIndex c sortedTemp
                        smallTemp = take (cPos+1) sortedTemp
                        newTemp = drop (cPos+1) sortedTemp
                        newTempCounts = increment c $! foldr decrement
                                                             tempCounts
                                                             smallTemp

