--import Data.List (sort)
import Data.Ratio ((%))

--main = print $ maximum [a % b | b <- [2..10000]
--                              , a <- [1..b-1]
--                              , gcd a b == 1
--                              , a*7 < b*3
--                              ]

main = print . maximum . map (\x -> (3*x `quot` 7 % x)) $ filter (\x -> not $ 0 == rem x 7) [2..1000000]