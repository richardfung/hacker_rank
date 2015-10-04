import Control.Applicative
import Control.Monad (forM_)
import Data.Graph
import Data.Set as Set (Set, fromList, member, toList)
import Debug.Trace

main = do
    [n, m] <- pure (map read) <*> ((pure words) <*> getLine) :: IO [Int]
    (romans, persians) <- genGraph n m
    print $ vertices persians

convertToVertex :: Int -> (Int, Int) -> Vertex
convertToVertex n (r, c) =
    let r' = r-1
        c' = c-1
    in r'*n + c'
                                
genGraph :: Int -> Int -> IO (Graph, Graph)
genGraph n m = do
    (rs, ps) <- getLists m True ([], [])
    let (rVertices, pVertices) = (Set.fromList rs, Set.fromList ps)
    return (setToGraph rVertices, setToGraph pVertices)
    where getEdges :: Int -> Set Int -> [Edge]
          getEdges v vertices = [(v, n) | n <- getNeighbors v vertices]
          getLists :: Int -> Bool -> ([Vertex], [Vertex])
                    -> IO ([Vertex], [Vertex])
          getLists 0 _ (rs, ps) = return (rs, ps)
          getLists i isRoman (rs, ps) = do
              [x,y] <- pure (map read)
                       <*> ((pure words) <*> getLine) :: IO [Int]
              let v = convertToVertex n (x,y) 
              if isRoman then getLists (i-1) (not isRoman) (v:rs, ps)
              else getLists (i-1) (not isRoman) (rs, v:ps)
          getNeighbors :: Int -> Set Int -> [Int]
          getNeighbors v vertices = filter (\x -> member x vertices)
                                           [v-n, v-n+1, v-1, v+1, v+n-1, v+n]
          setToGraph :: Set Int -> Graph
          setToGraph vertices =
              buildG (0, n*n-1) $ foldr (\v -> (++) (getEdges v vertices))
                                        [] $ toList vertices

traceShowId x = traceShow x x
