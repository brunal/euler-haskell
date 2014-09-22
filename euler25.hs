fibo :: [Integer]
fibo = 1 : 1 : zipWith (+) fibo (tail fibo)

main = print $ snd . head $ dropWhile ((<1000) . length . show . fst) $ zip fibo [1..] 