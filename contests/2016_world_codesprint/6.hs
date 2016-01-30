import Control.Monad (mapM)
import Control.Monad.ST
import Data.Array.ST
import Data.STRef

type Edge = Int
type Node = (Int, [Edge])

main = do
    q <- readLn :: IO Int
    ops <- mapM (\_ -> getLine) [1..q]
    print "blah"

-- genGraph :: [String] -> ST s (Array Int Node) -> ST s ()
-- genGraph ops 

node :: Int -> Node
node n = (n, [])

stepGraph :: [String] -> STArray s Int Node -> STRef s (Int) -> ST s ()
stepGraph [] _ _ = return ()
stepGraph (op:ops) sets kRef = case head op of
    'A' -> do k <- readSTRef kRef
              writeSTRef kRef $ k+1
              writeArray sets k $ node $ read $ (words op) !! 1
              return ()
    'B' -> do let opWords = words op
                  (x,y) = (read $! opWords !! 1, read $! opWords !! 2)
              (xCount, xEdges) <- readArray sets x
              (yCount, yEdges) <- readArray sets y
              writeArray sets x (xCount, y:xEdges)
              writeArray sets y (yCount, x:yEdges)
              return ()
    'C' -> return ()
