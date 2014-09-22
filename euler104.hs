import Data.List (sort)

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

pantadigital = (==) ['1'..'9'] . sort

main = print . fst . head . filter (doublePantadigital . snd) $ zip [0..] fibs

doublePantadigital :: Integer -> Bool
doublePantadigital n = digidi lasts && digidi firsts
                    where digidi f = pantadigital . show $ f 9 n

lasts :: Integer -> Integer -> Integer
lasts = flip rem . (10^)

firsts :: Integer -> Integer -> Integer
firsts n x | x >= 10^(n*n)  = firsts n $ x `div` (10^(n*n-n+1))
           | x >= 10^(2*n)  = firsts n $ x `div` (10^(n+1))
           | x >= 10^n  = firsts n $ x `div` 10
           | otherwise = x