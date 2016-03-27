import Control.Applicative ((<$>))
import Control.Monad (forM_, mapM)
import Data.Array.IO as A
import Data.IORef
import Data.List (find, isPrefixOf)

data Node = Empty | Node { children :: IOArray Int Node
                         , value :: IORef Int
                         , parent :: Node } deriving (Eq)

instance Show Node where
  show Empty = "Empty"
  show (Node c v p) = "Node"

data Instruction = ChangeValue Int
                 | Print
                 | VisitLeft
                 | VisitRight
                 | VisitParent
                 | VisitChild Int
                 | InsertLeft Int
                 | InsertRight Int
                 | InsertChild Int
                 | Delete
                 deriving (Show)

main = do
  q <- readLn :: IO Int
  ops <- mapM (\_ -> getLine) [1..q]
  cs <- newArray (1,10) Empty
  v <- newIORef 0
  processAll ops $ Node { children=cs
                        , value=v
                        , parent=Empty }

getInstruction :: String -> Instruction
getInstruction op =
  let ss = [ ("change", ChangeValue)
           , ("print", const Print)
           , ("visit left", const VisitLeft)
           , ("visit right", const VisitRight)
           , ("visit parent", const VisitParent)
           , ("visit child", VisitChild)
           , ("insert left ", InsertLeft)
           , ("insert right ", InsertRight)
           , ("insert child ", InsertChild)
           , ("delete", const Delete) ]
      (p, constructor) = head $ filter (\(a,b) -> isPrefixOf a op) ss
  in constructor $ read $ drop (length p) op

newNode :: Node -> Int -> IO Node
newNode p x = do newChildren <- newArray (1,10) Empty
                 newValue <- newIORef x
                 return Node { children=newChildren
                             , value=newValue
                             , parent=p }


processAll :: [String] -> Node -> IO ()
processAll [] _ = return ()
processAll (x:xs) n =
  let i = getInstruction x
  in do nextNode <- process i n
        processAll xs nextNode

process :: Instruction -> Node -> IO Node
process i n = case i of
  ChangeValue x -> do writeIORef (value n) x 
                      return n
  Print -> do x <- readIORef $ value n
              putStrLn $ show x
              return n
  VisitLeft -> do let siblings = children $ parent n
                  thisPos <- getPos siblings
                  readArray siblings $ thisPos-1
  VisitRight -> do let siblings = children $ parent n
                   thisPos <- getPos siblings
                   readArray siblings $ thisPos+1
  VisitParent -> return $ parent n
  VisitChild x -> readArray (children n) x
  InsertLeft x -> do let siblings = children $ parent n
                     thisPos <- getPos siblings
                     shiftRight siblings $ thisPos+1
                     node <- newNode (parent n) x
                     writeArray siblings thisPos node
                     return n
  InsertRight x -> do let siblings = children $ parent n
                      thisPos <- getPos siblings
                      shiftRight siblings $ thisPos+2
                      node <- newNode (parent n) x
                      writeArray siblings (thisPos+1) node
                      return n
  InsertChild x -> do let cs = children n
                      shiftRight cs 2
                      node <- newNode n x
                      writeArray cs 1 node
                      return n
  Delete -> do let p = parent n
                   siblings = children $ parent n
               thisPos <- getPos siblings
               forM_ [thisPos..9] $ \i -> do
                 temp <- readArray siblings $ i+1
                 writeArray siblings i temp
               return p
  where getPos :: IOArray Int Node -> IO Int
        getPos siblings = do fst <$> head <$> filter (\a -> snd a == n)
                               <$> getAssocs siblings
        shiftRight :: (Show e) => IOArray Int e -> Int -> IO ()
        shiftRight xs start = forM_ [10,9..start] $ \i -> do
                                temp <- readArray xs (i-1)
                                writeArray xs i temp
