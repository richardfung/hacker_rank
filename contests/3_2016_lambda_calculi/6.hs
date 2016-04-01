import Control.Applicative ((<$>), (<*>))
import Data.Array.IO
import Data.IORef

data MaxHeap = MaxHeap{ array::IORef (IOArray Int Int)
                      , size :: IORef Int }

main = do
  print "hi"

add :: Int -> MaxHeap -> IO ()
add n heap = do
  array' <- readIORef $ array heap
  cap <- (1+) <$> snd <$> getBounds array'
  curSize <- readIORef $ size heap
  if cap == curSize then increaseSize heap
  else return ()
  (readIORef $ array heap) >>= (\a -> writeArray a cap n)

bubbleUp :: MaxHeap -> Int -> IO ()
bubbleUp heap pos = do
  heapArray <- readIORef $ array heap
  p <- readArray heapArray (parent pos)
  val <- readArray heapArray pos
  if p < val then do writeArray heapArray pos p
                     writeArray heapArray (parent pos) val
                     bubbleUp heap $ parent pos
  else return ()

heap :: IO MaxHeap
heap = do
  array' <- newArray_ (0, 2^8) >>= newIORef 
  size' <- newIORef 0
  return MaxHeap {array=array', size=size'}

increaseSize :: MaxHeap -> IO ()
increaseSize heap = do
  array' <- readIORef $ array heap
  newSize <- (2 *) <$> ((1 +) <$> (snd <$> getBounds array'))
  newArray' <- newListArray (0, newSize-1) =<< getElems array'
  writeIORef (array heap) newArray'

max :: MaxHeap -> IO Int
max heap = (readIORef $ array heap) >>= (\array' -> readArray array' 0)

merge :: MaxHeap -> MaxHeap -> IO ()
merge from to = do
  es <- readIORef (array from) >>= getElems
  mapM_ (\i -> add i to) es
  s0 <- readIORef $ size from
  s1 <- readIORef $ size to
  newSize <- (+) <$> (readIORef $ size from) <*> (readIORef $ size to)
  writeIORef (size to) newSize

parent :: Int -> Int
parent n = (n-1) `div` 2

remove :: MaxHeap -> IO Int
remove heap =
