import System.Environment (getArgs)

main = do
    foo <- getArgs
    let length = read $ head foo
    print $ mem_fibs2 length + mem_fibs3 length + mem_fibs4 length
--    let tiles  = read . head $ tail foo
--    print . sum  $ map  (\x -> waysFib x length) tiles

ways :: Int -> Int -> Int
ways size length | size > length = 0
ways size length = length-size+1 + (sum $ map (ways size) [size..length-size])

waysFib :: Int -> Int -> Int
waysFib size length | size > length = 0
waysFib size length = 1 + (waysFib size $ length -1) + (waysFib size $ length-size)

memoized_fibs_n :: Int -> Int -> Integer
memoized_fibs_n n =
    let fibs_n p | n >  p = 0
        fibs_n p | n == p = 1
        fibs_n p = memoized_fibs_n n (p-n) + memoized_fibs_n n (p-1)
      in (map fibs_n [0..] !!)


mem_fibs2 :: Int -> Integer
mem_fibs2 =
    let fib 0 = 0
        fib 1 = 0
        fib p = 1 + mem_fibs2 (p-2) + mem_fibs2 (p-1)
      in (map fib [0..] !!)

mem_fibs3 :: Int -> Integer
mem_fibs3 =
    let fib 0 = 0
        fib 1 = 0
        fib 2 = 0
        fib p = 1 + mem_fibs3 (p-3) + mem_fibs3 (p-1)
      in (map fib [0..] !!)

mem_fibs4 :: Int -> Integer
mem_fibs4 =
    let fib 0 = 0
        fib 1 = 0
        fib 2 = 0
        fib 3 = 0
        fib p = 1 + mem_fibs4 (p-4) + mem_fibs4 (p-1)
      in (map fib [0..] !!)

