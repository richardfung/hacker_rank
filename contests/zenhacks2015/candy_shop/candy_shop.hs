main = do
    n <- readLn :: IO Int
    print $ solve n

--solve :: Int -> Int
solve n = count n 2

count 0 _ = 1
count _ 1 = 1
count n d = sum $ map (\x -> count x next) [n,n-d..0]
    where next = head $ dropWhile (>= d) [100,50,20,10,5,2,1]
