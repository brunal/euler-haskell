import Data.Ratio

diago = scanl (+) 1 . concat $ map (replicate 4) [2,4..]

main = print . sideLength $ ratioBelow (1, 10)

sideLength = (\(x:y:_) -> x - y + 1) . reverse

ratioBelow r = map snd . takeWhile ((\r' -> sup r' r) . fst) . drop 3 $ zip ratios diago
                where sup (num1, den1) (num2, den2) = num1*den2 >= num2*den1

ratios = scanl ratio (0, 1) $ tail diago
    where ratio (num,den) d = if isPrime d
                                then (1 + num, 1 + den)
                                else (num, 1 + den)



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