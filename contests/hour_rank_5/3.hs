import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Control.Monad.ST
import Data.List (foldl')
import Data.STRef

type Graph = (Int,Int,Int,Int)

modConst = 10^9 + 7

main = do
    _ <- readLn :: IO Int
    xs <- map read <$> words <$> getLine :: IO [Int]
    print $ solveST xs

genGraph :: [Int] -> Graph
genGraph xs =
    let helper :: [Int] -> Graph -> Graph
        helper [] g = seq g g
        helper (x:xs) g = helper xs $! next g x
    in helper xs singleton

solveST :: [Int] -> Graph
solveST xs = runST $ do
    endToEndRef <- newSTRef 0
    allToEndRef <- newSTRef 0
    totalRef <- newSTRef 0
    sizeRef <- newSTRef 1
    forM_ xs $ \x -> do
        endToEnd <- readSTRef endToEndRef
        allToEnd <- readSTRef allToEndRef
        total <- readSTRef totalRef
        size <- readSTRef sizeRef
        let dClose = (allToEnd + size*x) `mod` modConst
            dFar = (dClose + size*x) `mod` modConst
            nodeToEnd = (x + endToEnd) `mod` modConst
        writeSTRef endToEndRef $! (3*x + 2*endToEnd) `mod` modConst
        writeSTRef allToEndRef $!
                ((2*(dFar + nodeToEnd*size)) `mod` modConst)
                + ((2*x + endToEnd) `mod` modConst)
                + ((x + endToEnd) `mod` modConst)
                + ((dClose + nodeToEnd*size) `mod` modConst)
                + (allToEnd `mod` modConst)
        writeSTRef totalRef $!
                ((4*total) `mod` modConst)
                + x
                + (((4 + 8*size)*dClose) `mod` modConst)
                + (((4 + 4*size)*dFar) `mod` modConst)
        writeSTRef sizeRef $! (4*size + 2) `mod` modConst
    endToEnd <- readSTRef endToEndRef
    allToEnd <- readSTRef allToEndRef
    total <- readSTRef totalRef
    size <- readSTRef sizeRef
    return (endToEnd, allToEnd, total, size)

next :: Graph -> Int -> Graph
next (endToEnd, allToEnd,  total, size) x =
    let dClose = (allToEnd + size*x) `mod` modConst
        dFar = (dClose + size*x) `mod` modConst
        nodeToEnd = (x + endToEnd) `mod` modConst
        endToEnd' = (3*x + 2*endToEnd) `mod` modConst
        allToEnd' = ((2*(dFar + nodeToEnd*size)) `mod` modConst)
                  + ((2*x + endToEnd) `mod` modConst)
                  + ((x + endToEnd) `mod` modConst)
                  + ((dClose + nodeToEnd*size) `mod` modConst)
                  + (allToEnd `mod` modConst)
        total' = ((4*total) `mod` modConst)
               + x
               + (((4 + 8*size)*dClose) `mod` modConst)
               + (((4 + 4*size)*dFar) `mod` modConst)
        size' = (4*size + 2) `mod` modConst
    in ( (seq endToEnd' endToEnd') `mod` modConst
       , (seq allToEnd' allToEnd') `mod` modConst
       , (seq total' total') `mod` modConst
       , (seq size' size') `mod` modConst
       )

singleton :: Graph
singleton = (0,0,0,1)
