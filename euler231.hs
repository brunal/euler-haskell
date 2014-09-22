import System.Environment (getArgs)
--import Data.List ((\\))

main = do
    args <- getArgs
    let [k,n] = map read args
    print . sum $ factorsParmi k n

parmi k n = product [n-k+1..n] `quot` product [1..k]


factorsParmi k n =  primeFactors (product [n-k+1..n]) \\ primeFactors (product [1..k])

(\\) xs     []              = xs
(\\) (x:xs) (y:ys) | x == y = xs \\ ys
(\\) (x:xs) ys              = x : xs \\ ys


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
