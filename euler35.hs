isCircularPrime :: Int -> Bool
isCircularPrime = and . fmap isPrime . rotations

isPrime n = n == (last $ takeWhile (<= n) primes)

primes :: [Int]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]


rotations :: Int -> [Int]
rotations n = let nStr = show n
			  in map read $ rot (length nStr) nStr
	where rot i cs
		| i == 1	= [cs]
		| otherwise = cs:rot (i-1) ((tail cs) ++ [head cs])
		
		
main = print . length $ filter isCircularPrime [2..999999]