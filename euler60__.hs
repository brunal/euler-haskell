import Data.List (sort)

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


myPrimes = takeWhile (<300) primesTME     --limite arbitraire
mesSequences = combinationsOf 4 myPrimes

combinationsOf 0 _ = [[]]
combinationsOf _ [] = []
combinationsOf k (x:xs) = map (x:) (combinationsOf (k-1) xs) ++ combinationsOf k xs

concatenations [] = []
concatenations (x:xs) = sort $ map (conca x) xs ++ map (flip conca x) xs ++ concatenations xs

premiers xs = allin xs primesTME
    where allin [] _ = True
          allin (x:xs) (y:ys) = case (compare x y) of
                    LT -> False
                    EQ -> allin xs ys
                    GT -> allin (x:xs) ys

problem60 = filter (premiers . concatenations) mesSequences

main = print problem60