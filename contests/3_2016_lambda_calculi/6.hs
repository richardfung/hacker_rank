import Data.Array.IO
import Data.IORef

data MaxHeap = { array::IORef (IOArray Int Int)
               , size :: IO Int }

add :: Int -> MaxHeap -> IO ()
add n heap =
  if snd (getBounds heap) + 1 == size heap then

heap :: IO MaxHeap
heap = do
  array' <- newIORef $ newArray_ (0, 2^8)
  return MaxHeap {array=array', size=0}

max :: MaxHeap -> IO Int

merge :: MaxHeap -> MaxHeap -> MaxHeap

remove :: MaxHeap -> IO Int

increaseSize :: MaxHeap -> IO ()
increaseSize heap = do
  array <- readIORef $ array heap
  newSize <- 2 * (1 + (snd $ getBounds array))
  newArray' = newArray (0, newSize-1) 
