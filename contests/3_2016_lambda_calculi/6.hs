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
       bubbleUp heap n

bubbleUp :: MaxHeap -> Int -> IO ()

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

merge :: MaxHeap -> MaxHeap -> MaxHeap

parent :: Int -> Int
parent n = (n-1) `div` 2

remove :: MaxHeap -> IO Int
