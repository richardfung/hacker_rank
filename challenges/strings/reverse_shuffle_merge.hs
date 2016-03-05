import Data.List (elemIndex, sort)
import Data.Map.Strict as M
import Data.Maybe
import Debug.Trace

main = do
    s <- getLine
    putStrLn $! answer s

get :: (Ord k) => k -> Map k Int -> Int
get k m = findWithDefault 0 k m

increment :: (Ord k) => k -> Map k Int -> Map k Int
increment k m = insert k (get k m + 1)  m

answer :: String -> String
answer s = helper (reverse s) "" M.empty totals []
    where helper :: String -> String -> Map Char Int -> Map Char Int -> [String]
                    -> String
          helper [] _ _ _ revAcc = concat $! reverse revAcc
          helper (c:cs) temp counts totals revAcc
              | count < total =
                  helper cs (c:temp) (increment c counts) totals revAcc
              | otherwise =
                  let sortedTemp = sort (c:temp)
                      goodTemp = take (fromJust (elemIndex c sortedTemp) + 1)
                                      sortedTemp
                      newRevAcc = goodTemp : revAcc
                  in helper cs "" (increment c counts) totals newRevAcc
              where count = get c counts
                    total = totals ! c 
          totals = M.map (`div` 2) $! Prelude.foldr (\k -> increment k)
                                                          M.empty s
