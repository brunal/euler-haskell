triangle :: [Integer]
triangle = scanl1 (+) [1..]
--triangle = [x*(x+1) `div` 2 | x <- [0..]]

divisors :: Integer -> Int
divisors n = divide n 1
        where divide n p = if p^2 > n
                                then 0
                                else (if n `mod` p == 0
                                        then 2          -- p and n/p
                                        else 0) + (divide n (p+1))

main = print $ head $ filter (\n -> divisors n > 500) triangle
