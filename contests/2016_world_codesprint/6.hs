import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.ST
import Data.Array as A
import Data.Array.ST
import Data.Map as M
import Data.Set as S
import Data.STRef
import Debug.Trace

type Edge = Int
type Node = (Int, [Edge])

main = do
    q <- readLn :: IO Int
    ops <- mapM (\_ -> getLine) [1..q]
    print $ fst $ combine $ genGraph ops

genGraph :: [String] -> Map Int Node
genGraph ops =
    let array = runSTArray $ do
            kRef <- newSTRef 1
            sets <- newArray_ (1, countSets ops) 
            stepGraph ops sets kRef
    in Prelude.foldr (\(k,v) -> M.insert k v) M.empty $ A.assocs array

combine :: Map Int Node -> Node
combine m
    | M.null m = (0, [])
    | otherwise =
          let removeNeighbors :: Map Int Node -> [Edge] -> Map Int Node
              removeNeighbors m ns =
                  Prelude.foldr (\n -> \m -> M.delete n m) m ns
              setSize :: Map Int Node -> STRef s (Map (Map Int Node) Int) -> ST s Int
              setSize m prev = do
                  answers <- readSTRef prev
                  if M.member m answers then return $! answers M.! m
                  else do ans <- mapM (\k ->
                                     let (p,es) = m M.! k
                                         newM = removeNeighbors (M.delete k m) es
                                     in (p+) <$> setSize newM prev
                                      ) $! keys m
                          let max_ = if Prelude.null ans then 0 else maximum ans
                          modifySTRef prev (M.insert m max_)
                          return max_
          in runST $ do
                 answers <- newSTRef M.empty
                 n <- setSize m answers
                 return (n,[])

countSets :: [String] -> Int
countSets [] = 0
countSets (op:ops) = case head op of
    'A' -> 1 + countSets ops
    'B' -> countSets ops
    'C' -> 1 + countSets ops

node :: Int -> Node
node n = (n, [])

stepGraph :: [String] -> STArray s Int Node -> STRef s (Int) -> ST s (STArray s Int Node)
stepGraph [] sets _ = return sets
stepGraph (op:ops) sets kRef = case head op of
    'A' -> do k <- readSTRef kRef
              modifySTRef kRef (+1)
              writeArray sets k $ node $ read $ (words op) !! 1
              stepGraph ops sets kRef
    'B' -> do let opWords = words op
                  (x,y) = (read $! opWords !! 1, read $! opWords !! 2)
              (xCount, xEdges) <- readArray sets x
              (yCount, yEdges) <- readArray sets y
              writeArray sets x (xCount, y:xEdges)
              writeArray sets y (yCount, x:yEdges)
              stepGraph ops sets kRef
    'C' -> do let x = read $ (words op) !! 1
              node <- readArray sets x
              stack <- newSTRef [x]
              visited <- newSTRef S.empty
              nodeMap <- newSTRef M.empty
              whileM (not <$> Prelude.null <$> readSTRef stack) $ do
                  pos <- head <$> readSTRef stack
                  node <- readArray sets pos
                  modifySTRef stack tail
                  modifySTRef visited (S.insert pos)
                  modifySTRef nodeMap (M.insert pos node)
                  writeArray sets pos (0, [])
                  mapM (\e -> do isVisited <- S.member e <$> readSTRef visited 
                                 if isVisited then return ()
                                 else do modifySTRef visited (S.insert e)
                                         modifySTRef stack (e:)
                       ) $ snd node
              combined <- combine <$> readSTRef nodeMap
              k <- readSTRef kRef
              modifySTRef kRef (+1)
              writeArray sets k combined
              stepGraph ops sets kRef

whileM :: (Monad m) => m Bool -> m a -> m [a]
whileM p f = go
    where go = do
              x <- p
              if x then do
                  x <- f
                  xs <- go
                  return (return x `mplus` xs)
              else return mzero
