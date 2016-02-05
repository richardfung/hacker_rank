import Control.Applicative ((<$>))
import Data.List (foldl')

type Graph = (Int,Int,Int,Int)

modConst = 10^9 + 7

main = do
    _ <- readLn :: IO Int
    xs <- map read <$> words <$> getLine :: IO [Int]
    let (_,_,total,_) =  genGraph xs
    print total

genGraph :: [Int] -> Graph
genGraph xs =
    let helper :: [Int] -> Graph -> Graph
        helper [] g = seq g g
        helper (x:xs) g = helper xs $! next g x
    in helper xs singleton

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
