
--suMul :: Int -> Int
sumMul x = sum $ filter (\n -> n `mod` 3 == 0 || n `mod` 5 == 0) [1..(x-1)]

main = putStrLn . show $ sumMul 1000
