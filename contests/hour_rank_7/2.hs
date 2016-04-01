import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Array

main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \i -> do
    n <- readLn :: IO Int
    ns <- map read <$> words <$> getLine
    let leftSums = listArray (-1, n-1) $ scanl (+) 0 ns
    putStrLn $ show $ maxPoints leftSums n

maxPoints :: Array Int Int -> Int -> Int
maxPoints leftSums n = (helper 0 $ n-1) - 1
  where helper :: Int -> Int -> Int
        helper start end =
          let ps = filter (\l -> sum' start l == sum' (l+1) end) [start..end-1]
              lrs = map (\p -> ((start, p), (p+1, end))) ps
              maxes = map (\((left), (right)) ->
                           max (answers ! left) (answers ! right)
                          ) lrs
              best = maximum (0:maxes)
          in 1 + best
        sum' start end = (leftSums ! end) - (leftSums ! (start-1))
        answers :: Array (Int, Int) Int
        answers = listArray answersBounds $ map (\(i,j) -> helper i j)
                                                $ range answersBounds
        answersBounds = ((0,0), (n-1, n-1))
