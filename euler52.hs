import Data.List (sort)

main = print $ euler52 1 1 []

euler52 :: Int -> Int -> [Char] -> Int
euler52 n 1 _ = euler52 n 2 . sort $ show n
euler52 n 7 _ = n
euler52 n p cs 
        | (sort . show $ n*p) == cs = euler52 n (p+1) cs
        | otherwise               = euler52 (n+1) 1 []
