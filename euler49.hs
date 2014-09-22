import Data.List ((\\), nub, sort, delete, concat, subsequences)

myPrimes = takeWhile (<=9998) $ dropWhile (<=1000) primes 

three = nub $ two \\ (nub two)
    where two = digits \\ (nub digits)
          digits = map (sort . show) myPrimes

main = print . filter isSequence . concat . map (subgroups 3) $ map primesInThree three
    where primesInThree t = filter (\p -> (sort . show) p == t) myPrimes
          byThree xs = xs
          isCandidate = (==3) . length
          isSequence [a,b,c] = c-b == b-a
          subgroups n = filter ((==n) . length) . subsequences  
    
primes :: [Int]
primes = 2 : sieve [3,5..]  where
    sieve []     = []
    sieve (p:xs) = p : sieve (xs `minus` [p*p,p*p+2*p..])

minus (x:xs) (y:ys) = case (compare x y) of
          LT -> x : minus  xs  (y:ys)
          EQ ->     minus  xs     ys
          GT ->     minus (x:xs)  ys
minus  xs     _     = xs