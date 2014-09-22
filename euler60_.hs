import Data.List (subsequences)

conca :: Int -> Int -> Int
conca x y = y + x * 10^(length . show) y
--conca x y = read $ show x ++ show y

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


myPrimes = takeWhile (<250) primesTME     --limite arbitraire
mesSequences = combinationsOf 3 myPrimes

myElem :: (Ord a) => a -> [a] -> Bool
myElem x (y:ys) = case (compare x y) of
        LT -> False
        EQ -> True
        GT -> myElem x ys

tous :: (a -> a -> Bool) -> [a] -> Bool
tous _ [] = True
tous f (x:xs) = all (f x) xs && all (flip f x) xs && tous f xs

combinationsOf 0 _ = [[]]
combinationsOf _ [] = []
combinationsOf k (x:xs) = map (x:) (combinationsOf (k-1) xs) ++ combinationsOf k xs

combinaisonPremiere x y = (conca x y) `myElem` primesTME

problem60 = filter (tous combinaisonPremiere) mesSequences

main = print problem60