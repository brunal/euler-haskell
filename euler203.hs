import Data.List (nub)

cleanrow n = filter (notdiv squares) $ map (choose n) [1..n `quot` 2]   --no need to go after half: it's symetric. No need to start at 0: it's always 1
    where choose n k = product [n-k+1..n] `quot` product [1..k]

notdiv (x:xs) y | x > y        = True
                | y `rem` x == 0 = False
                | otherwise      = notdiv xs y

main = print . sum . (1:) . nub . concat $ map cleanrow [0..50]

squares = map (\x -> x*x) primesTME
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