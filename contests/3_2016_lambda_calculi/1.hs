import Control.Applicative ((<$>))
import Control.Monad (forM_, mapM)
import Data.Map as M

main = do
  t <- readLn :: IO Int
  forM_ [1..t] $ \_ -> do
    n <- readLn :: IO Int
    pairs <- mapM (\_ -> (\[a,b] -> (read a, read b)) <$> words <$> getLine)
                  [1..n] :: IO [(Int, Int)]
    let answer = if validFunction pairs then "YES" else "NO"
    putStrLn answer

validFunction :: [(Int, Int)] -> Bool
validFunction pairs = helper pairs M.empty
  where helper :: [(Int, Int)] -> Map Int Int -> Bool
        helper [] _ = True
        helper ((k,v):xs) m =
          if member k m then if m ! k == v then helper xs m
                             else False
          else helper xs $ insert k v m
