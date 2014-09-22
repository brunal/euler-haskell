--Aurait pu etre utilise pour le probleme 3
primeFactors n = factor n primes
	where factor n (p:ps)
		| p^2 > n			= [n]
		| n `mod` p == 0	= p:factor (n `div` p) (p:ps)
		| otherwise			= factor n ps
	

primes = 2:filter ((==1) . length . primeFactors) [3,5..]

main = print $ primes !! 10000