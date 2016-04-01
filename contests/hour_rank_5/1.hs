main = do
    n <- readLn :: IO Int
    let smallest = n `div` 3
        a = smallest
        b = if smallest * 3 < n then smallest + 1 else smallest
        c = if smallest * 3 + 1 < n then smallest + 1 else smallest
    print $ a*b*c
