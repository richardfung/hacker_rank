import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_, mapM_)
import Data.Array as A
import Data.Array.IO
import Data.IORef

data MaxHeap = MaxHeap{ backing::IORef (IOArray Int Int)
                      , size :: IORef Int }

main = do
  [n,q] <- map read <$> words <$> getLine :: IO [Int]
  armies <- listArray (1, n) <$> mapM (\_ -> heap) [1..n]
  forM_ [1..q] $ \i -> do
    command <- map read <$> words <$> getLine :: IO [Int]
    runCommand command armies

runCommand :: [Int] -> A.Array Int MaxHeap -> IO ()
runCommand [1, n] armies = (peek $ armies ! n) >>= (\i -> putStrLn $ show i)
runCommand [2, n] armies = (remove $ armies ! n) >>= (const $ return ())
runCommand [3, n, c] armies = add c $ armies ! n
runCommand [4, i, j] armies = merge (armies ! j) (armies ! i)

add :: Int -> MaxHeap -> IO ()
add n heap = do
  backing' <- readIORef $ backing heap
  cap <- (1+) <$> snd <$> getBounds backing'
  curSize <- readIORef $ size heap
  if cap == curSize then increaseSize heap
  else return ()
  (readIORef $ backing heap) >>= (\a -> writeArray a curSize n)
  modifyIORef' (size heap) (+1)
  bubbleUp heap curSize

bubbleUp :: MaxHeap -> Int -> IO ()
bubbleUp heap pos = do
  heapArray <- readIORef $ backing heap
  p <- readArray heapArray (parent pos)
  val <- readArray heapArray pos
  if p < val then do writeArray heapArray pos p
                     writeArray heapArray (parent pos) val
                     bubbleUp heap $ parent pos
  else return ()

children :: Int -> (Int, Int)
children n = (n*2 + 1, n*2 + 2)

heap :: IO MaxHeap
heap = do
  backing' <- newArray_ (0, 2^8) >>= newIORef 
  size' <- newIORef 0
  return MaxHeap {backing=backing', size=size'}

increaseSize :: MaxHeap -> IO ()
increaseSize heap = do
  backing' <- readIORef $ backing heap
  newSize <- (2 *) <$> ((1 +) <$> (snd <$> getBounds backing'))
  newArray' <- newListArray (0, newSize-1) =<< getElems backing'
  writeIORef (backing heap) newArray'

merge :: MaxHeap -> MaxHeap -> IO ()
merge from to = do
  s0 <- readIORef $ size from
  s1 <- readIORef $ size to
  es <- take s0 <$> (readIORef (backing from) >>= getElems)
  mapM_ (\i -> add i to) es
  writeIORef (backing from) =<< (newArray_ (0,0))
  writeIORef (size to) (s0+s1)

parent :: Int -> Int
parent 0 = 0
parent n = (n-1) `div` 2

peek :: MaxHeap -> IO Int
peek heap = (readIORef $ backing heap) >>= (\backing' -> readArray backing' 0)

remove :: MaxHeap -> IO Int
remove heap = do
  last <- (-1 +) <$> readIORef (size heap)
  backing' <- readIORef (backing heap)
  lastElement <- readArray backing' last
  ret <- readArray backing' 0
  modifyIORef' (size heap) (-1 +)
  writeArray backing' 0 lastElement
  trickleDown heap 0
  return ret

trickleDown :: MaxHeap -> Int -> IO ()
trickleDown heap n = do
  backing' <- readIORef (backing heap)
  val <- readArray backing' n
  size' <- readIORef $ size heap
  let (l,r) = children n
  if l < size' then do
    if r == size' then do let r = l
                          return ()
    else return ()
    [left,right] <- mapM (readArray backing') [l,r]
    let max' = maximum [val, left, right]
    if max' /= val then do let maxPos = if max' == left then l else r
                           writeArray backing' maxPos val
                           writeArray backing' n max'
                           trickleDown heap maxPos
    else return ()
  else return ()
