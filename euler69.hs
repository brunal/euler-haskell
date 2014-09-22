import Data.List (maximumBy)
import Data.Function (on)
import Data.Ratio ((%))
import Control.Applicative

--totient n = length $ filter ((==1) . (gcd n)) [1..n]
--ratio n = n % totient n

--main = print . maximumBy (compare `on` fst) $ zip <$> map ratio <*> id $ [1..10000]   --10k -> 0'42
--main = print . maximum $ zip <$> map ratio <*> id $ [1..10000]    --10k -> 0'38
--main = print $ maximumBy (compare `on` ratio) [1..10000]  --10k -> 2'15

main = print . last . takeWhile (<1000000) $ scanl (*) 1 primesTME  --1M -> 0'00"4, 80ms if compiled


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