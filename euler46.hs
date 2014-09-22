twiceSquares = map ((*2) . (^2)) [1..]

isGoldbach :: Int -> Bool
isGoldbach n = null . filter isPrime . takeWhile (>1) $ map (n-) twiceSquares

oddComposite = filter (not . isPrime) $ map ((+1) . (*2)) [1..]

main = print . head $ dropWhile (not . isGoldbach) oddComposite

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