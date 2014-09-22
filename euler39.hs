import Monad (guard)
import Data.List (maximumBy)

right :: Int -> Int
right p = length $ do
    a <- [p `rem` 3..p-2]
    b <- [(p-a) `rem` 2..p-a-1]
    let c = p - a - b
    guard (a > b && b >= c)
    guard (a^2 == b^2 + c^2)
    return (a, b, c)
    
main = print . maximumBy (\(n,p) (n',p') -> p `compare` p') . zip [1..1000] $ map right [1..1000]