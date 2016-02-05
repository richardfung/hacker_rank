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
    print $ total $ foldl' next singleton xs

next :: Graph -> Int -> Graph
next g x =
    let dClose = (allToEnd g) + (size g)*x
        dFar = dClose + (size g)*x
        nodeToEnd = (x + (endToEnd g))
        endToEnd' = (3*x + 2*(endToEnd g))
        allToEnd' = 2*(dFar + nodeToEnd*(size g))
                  + (2*x + (endToEnd g))
                  + (x + (endToEnd g))
                  + (dClose + nodeToEnd*(size g))
                  + (allToEnd g)
        total' = ((4*(total g)))
               + x
               + (4 + 8*(size g))*dClose
               + (4 + 4*(size g))*dFar
        size' = (4*(size g) + 2)
    in Graph { endToEnd=endToEnd' `mod` modConst
             , allToEnd=allToEnd' `mod` modConst
             , total = total' `mod` modConst
             , size=size' `mod` modConst }

singleton :: Graph
singleton = Graph { endToEnd=0, allToEnd=0, size=1, total=0 }
