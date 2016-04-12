import Data.List as L
import Data.Map as M
import Debug.Trace

type Count = Map Char Int

main = do
  s <- getLine
  putStrLn $ findSmallest $ reverse s

decrement :: Char -> Count -> Count
decrement c count = M.insert c (count ! c - 1) count

findSmallest :: String -> String
findSmallest s = helper s M.empty [] counts counts
  where helper :: String -> Count -> String -> Count -> Count -> String
        helper [] bufferCount _ _ shuffleCount = ""
        helper (c:cs) bufferCount revBuffer aCount shuffleCount =
          if required bufferCount shuffleCount c then
            let (added, unadded, newBuffer) =
                  section aCount (reverse $ c:revBuffer) c
                newBufferCount =
                  increment c $ L.foldr decrement (L.foldr decrement
                                                           bufferCount
                                                           added
                                                  ) unadded
                newRevBuffer = reverse newBuffer
                newACount = L.foldr decrement aCount added
                newShuffleCount = L.foldr decrement shuffleCount unadded 
            in added ++ (helper cs newBufferCount newRevBuffer newACount
                                newShuffleCount)
          else helper cs (increment c bufferCount) (c:revBuffer) aCount
                      shuffleCount 
        counts = M.map (`div` 2) $ L.foldr increment M.empty s

findDefZero :: Char -> Count -> Int
findDefZero c count = M.findWithDefault 0 c count

increment :: Char -> Count -> Count
increment c count = M.insert c (M.findWithDefault 0 c count + 1) count

required :: Count -> Count -> Char -> Bool
required bufferCount shuffleCount c =
  (shuffleCount ! c) == (findDefZero c bufferCount)

section :: Count -> String -> Char -> (String, String, String)
section left cs mustAdd =
  let smallest = smallestValid left cs
      newLeft = decrement smallest left
      n = head $ elemIndices smallest cs
      remaining = drop (n+1) cs
      (fstRest, sndRest, thdRest) = section newLeft remaining mustAdd
      untaken = take n cs
  in if smallest == mustAdd then ([smallest], untaken, remaining)
     else (smallest:fstRest, untaken ++ sndRest, thdRest)

smallestValid :: Count -> String -> Char
smallestValid left cs = minimum $ L.filter (\x -> left ! x > 0) cs
