import Control.Applicative ((<$>))
import Data.Map as M

type Graph = Map Char [(Char,Int)]

main = do
    edges <- Prelude.map read <$> words <$> getLine :: IO [Int]
    print $! buildGraph edges

buildGraph :: [Int] -> Graph
buildGraph [a,b,c,d,e,f] =
    M.fromList [ ('a', [('b', b), ('c', e)])
               , ('b', [('c', c), ('d', f)])
               , ('c', [('d', d)])
               , ('d', [('a', a)])]

cycles :: Graph -> Char -> [Int]
