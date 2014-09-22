import Data.List
import Data.Char (digitToInt)

--Bad implementation of primes and isPrime

primes :: [Integer]
primes = 2 : sieve [3,5..]  where
    sieve []     = []
    sieve (p:xs) = p : sieve (xs `minus` [p*p,p*p+2*p..])

minus (x:xs) (y:ys) = case (compare x y) of
          LT -> x : minus  xs  (y:ys)
          EQ ->     minus  xs     ys
          GT ->     minus (x:xs)  ys
minus  xs     _     = xs

isPrime n = let primes' = takeWhile (<=n) primes
            in if length primes' == 0
                then False
                else last primes' == n


isCool :: Integer -> Bool
isCool n = isCool' n 10 (mod) && isCool' n 10 (div)

isCool' n p f 
    | p > n     = True
    | otherwise = isPrime (f n p) && isCool' n (10*p) f

--isCool :: Integer -> Bool
--isCool n = (foo fromLeft) && (foo fromRight)
--        where foo f = and $ map isPrime . f $ show n

--fromLeft :: String -> [Integer]
--fromLeft = map read . tail . inits

--fromRight :: String -> [Integer]
--fromRight = map (read . reverse) . tail . inits . reverse


main = print . sum . take 11 . filter isCool . dropWhile (<=7) $ primes
--main = print . take 11 . filter isCool . dropWhile (<=7) $ primes