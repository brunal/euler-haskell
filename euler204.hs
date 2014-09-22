import Control.Applicative ((<*>),(<$>))
import Data.List (nub)
import System.Environment (getArgs)

main = do
    foo <- getArgs
    let type_ = read $ head foo
    let max = read . head $ tail foo
    print . (+1) . sum . map length $ hammings type_ (10^max)

hammings :: Int -> Int -> [[Int]]
hammings n maxi = let primes = takeWhile (<= n) primesTME
                      multiplox = ((*) <$> primes <*>)
                      nextStep = nub . (filter (<=maxi)) . multiplox
                    in takeWhile (not . null) $ iterate nextStep primes


primeFactors :: Int -> [Int]
primeFactors n = factors n primesTME
    where factors :: Int -> [Int] -> [Int]
          factors n (x:xs) = if n == 1
                                then []
                                else if n `rem` x == 0
                                        then x : factors (n `quot` x) (x:xs)
                                        else factors n xs

isPrime :: Int -> Bool
isPrime k = k > 1 &&
   foldr (\p r -> p*p > k || k `rem` p /= 0 && r)
      True primesTME

primesTME = 2 : gaps 3 (join [[p*p,p*p+2*p..] | p <- primes'])
  where
    primes' = 3 : gaps 5 (join [[p*p,p*p+2*p..] | p <- primes'])
    join  ((x:xs):t)        = x : union xs (join (pairs t))
    pairs ((x:xs):ys:t)     = (x : union xs ys) : pairs t
    gaps k xs@(x:t) | k==x  = gaps (k+2) t 
                    | True  = k : gaps (k+2) xs

union (x:xs) (y:ys) = case (compare x y) of 
           LT -> x : union  xs  (y:ys)
           EQ -> x : union  xs     ys 
           GT -> y : union (x:xs)  ys