collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n | n `mod` 2 == 0 = n : collatz (n `div` 2)
collatz n = n : collatz (3*n+1)

problem14 = maximum . map (\x -> (length . collatz $ x, x)) $ [1..999999]

main :: IO ()
main = putStrLn . show . snd $ problem14
