import System.Environment (getArgs)

main = do
    foo <- getArgs
    let length = read $ head foo
    print $ mem_fibs length

mem_fibs :: Int -> Integer
mem_fibs =
    let fib 0 = 1
        fib 1 = 1
        fib 2 = 2
        fib 3 = 4
        fib p = mem_fibs (p-1) + mem_fibs (p-2) + mem_fibs (p-3) + mem_fibs (p-4)
      in (map fib [0..] !!)
