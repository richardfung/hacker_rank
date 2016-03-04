import Control.Applicative ((<$>))
import Control.Monad (mapM)
import Data.Map as M

main = do
    t <- readLn :: IO Int
    ss <- mapM (\_ -> getLine) [1..t]
    print ss

getAnagramSets :: String -> Map String Int
