import Control.Applicative

main = do
    [n, d] <- (pure $ map read) <*> ((pure words) <*> getLine) :: IO [Int]
    print $ (ceiling $ (fromIntegral n) / 2) + d
