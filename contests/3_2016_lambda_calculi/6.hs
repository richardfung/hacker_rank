import Data.Array.IO
import Data.IORef

data MaxHeap = { array::IORef (IOArray Int Int)
               , size :: IO Int }

add :: Int -> MaxHeap -> IO ()
add n heap =
  if snd (getBounds heap) + 1 == size heap then
    do increaseSize heap
       array <- readIORef $ array heap
       writeArray array (size heap) n
       modifyIORef' (+1) $ size heap
       bubbleUp heap $ size heap - 1

bubbleUp :: MaxHeap -> Int -> IO ()
bubbleUp heap pos = do
  p <- readArray (array heap) (parent pos)
  val <- readArray (array heap) pos
  if p < val then do writeArray (array heap) pos p
                     writeArray (array heap) (parent pos) val
                     bubbleUp heap $ parent pos
  else return ()

heap :: IO MaxHeap
heap = do
  array' <- newIORef $ newArray_ (0, 2^8)
  return MaxHeap {array=array', size=0}

increaseSize :: MaxHeap -> IO ()
increaseSize heap = do
  array <- readIORef $ array heap
  newSize <- 2 * (1 + (snd $ getBounds array))
  newArray' <- newListArray (0, newSize-1) $ getElems array
  writeIORef (array heap) newArray'

max :: MaxHeap -> IO Int
max heap = readArray (array heap) 0

merge :: MaxHeap -> MaxHeap -> IO ()

parent :: Int -> Int
parent n = (n-1) `div` 2

remove :: MaxHeap -> IO Int
