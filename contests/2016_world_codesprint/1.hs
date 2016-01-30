main = do
    s <- getLine
    print $ count s

count :: String -> Int
count s =
    let helper :: String -> String -> Int
        helper [] _ = 0
        helper (s:ss) (m:ms) =
            let rest = helper ss ms
                add = if s /= m then 1 else 0
            in add + rest
    in helper s $ concat $ repeat "SOS"
