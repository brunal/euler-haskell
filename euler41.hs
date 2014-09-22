import Data.List ((\\))
import Data.Char (intToDigit)
import Data.List (permutations)
import Monad (guard)

main = print . maximum $ do
    d <- [3..9]
    n <- permutations ['1'..intToDigit d]
    let n' = read n
    guard (isPrime n')
    return n'
    
isPrime x = not $ any divisible $ takeWhile notTooBig [2..] where
     divisible y = x `mod`y == 0
     notTooBig y = y*y <= x