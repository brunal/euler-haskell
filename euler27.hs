import Data.List (lookup)

quad :: Int -> Int -> Int -> Int
quad a b n = n^2 + a*n + b

consecutivePrimes :: Int -> Int -> Int
consecutivePrimes a b = isPrime a b 0
	where isPrime a b n
		| quad a b n <= 1 = 0
		| isPrime' (quad a b n) = 1 + isPrime a b (n+1)
		| otherwise = 0
		
isPrime' :: Int -> Bool
isPrime' x = x == (last $ takeWhile (<=x) primes)

primes :: [Int]
primes = sieve [2..]
	where sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]
	
findM :: (Eq a) => ([a] -> a) -> [(a, b)] -> b
findM f list =  let best = (f . fst . unzip) list
				in case lookup best list of
					Just b  -> b
					Nothing -> undefined

main = (print . findM maximum) [(consecutivePrimes a b, a*b) | b <- [2..999], a <- [1-b..999]]	-- no need to sart b before - see n = 0 ; and see n=1 for a