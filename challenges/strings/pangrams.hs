import Data.Bits
import Data.Char
import Data.Maybe

main = do
    sentence <- getLine
    if isPangram sentence then putStrLn "pangram"
    else putStrLn "not pangram"

isPangram :: String -> Bool
isPangram xs =  helper xs zeroBits
    where helper :: String -> Int -> Bool
          helper [] bits = popCount bits == 26
          helper (x:xs) bits =
              let pos = ord (toLower x) - (ord 'a')
                  newBits = if pos >= 0 && pos <= 26 then setBit bits pos
                            else bits
              in helper xs newBits
