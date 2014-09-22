import Data.Ratio
import Data.List (sort, foldl1')

fractions :: Int -> Int
fractions d =  length $ filter (\n -> gcd n d == 1) [d `quot` 3 + 1..d `quot` 2]

main = print . (\x -> x-1) . sum $ map fractions [1..12000]