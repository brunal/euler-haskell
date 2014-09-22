import Data.Ratio
import Data.List (sort, foldl1')

fractions :: Int -> Int
fractions d =  length $ filter (\n -> gcd n d == 1) [1..d-1]

main = print . sum $ map fractions [1..1000000]