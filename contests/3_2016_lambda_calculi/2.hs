import Control.Applicative ((<$>))
import Control.Monad (mapM)

main = do
  n <- readLn :: IO Int
  let addToEnd :: [a] -> [a]
      addToEnd xs = xs ++ [head xs]
  ls <- lengths <$> addToEnd <$> mapM (\_ -> (\[a,b] -> (read a, read b))
                                             <$> words <$> getLine
                                      ) [1..n] :: IO [Double]
  putStrLn $ show $ sum ls

lengths :: [(Int, Int)] -> [Double]
lengths [x] = []
lengths (a:b:cs) =
  (sqrt $ fromIntegral $ (fst a - fst b)^2 + (snd a - snd b)^2):(lengths $ b:cs)
