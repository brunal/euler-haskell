import System.Environment (getArgs)

p :: Int -> Int  -> Int
p 0 _ = 1
p n max = sum $ map (\i -> p (n-i) i) [1..min n max]
arrangements :: Int -> Int
arrangements n = p n n

main = do
    args <- getArgs
    let goal = read $ head args
    print . head $ filter ((== 0) . (`mod` goal) . arrangements) [1..]