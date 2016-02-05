import Control.Applicative ((<$>))
import Data.List (foldl')

data Graph = Graph { endToEnd :: Int
                   , allToEnd :: Int
                   , size :: Int
                   , total :: Int } deriving (Show)

modConst = 10^9 + 7

main = do
    _ <- readLn :: IO Int
    xs <- map read <$> words <$> getLine :: IO [Int]
    print $! total $! genGraph xs

genGraph :: [Int] -> Graph
genGraph xs =
    let helper :: [Int] -> Graph -> Graph
        helper [] g = seq g g
        helper (x:xs) g = helper xs $! next g x
    in helper xs singleton

next :: Graph -> Int -> Graph
next g x =
    let dClose = ((allToEnd g) + (size g)*x) `mod` modConst
        dFar = (dClose + (size g)*x) `mod` modConst
        nodeToEnd = (x + (endToEnd g)) `mod` modConst
        endToEnd' = (3*x + 2*(endToEnd g)) `mod` modConst
        allToEnd' = ((2*(dFar + nodeToEnd*(size g))) `mod` modConst)
                  + ((2*x + (endToEnd g)) `mod` modConst)
                  + ((x + (endToEnd g)) `mod` modConst)
                  + ((dClose + nodeToEnd*(size g)) `mod` modConst)
                  + ((allToEnd g) `mod` modConst)
        total' = ((4*(total g)) `mod` modConst)
               + x
               + (((4 + 8*(size g))*dClose) `mod` modConst)
               + (((4 + 4*(size g))*dFar) `mod` modConst)
        size' = (4*(size g) + 2) `mod` modConst
    in Graph { endToEnd=(seq endToEnd' endToEnd') `mod` modConst
             , allToEnd=(seq allToEnd' allToEnd') `mod` modConst
             , total = (seq total' total') `mod` modConst
             , size=(seq size' size') `mod` modConst }

singleton :: Graph
singleton = Graph { endToEnd=0, allToEnd=0, size=1, total=0 }
