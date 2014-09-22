import Monad (guard)
import Data.Ratio ((%))

unorthodox = do
    a <- t
    b <- t
    c <- t
    guard (a /= c)
    guard (a % c == (10*a+b) % (10*b+c))
    return (a%c)
        where t = [1..9]

main = print . show . product $ unorthodox