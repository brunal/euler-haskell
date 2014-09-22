largestPrimeDiv :: (Integral a) => a -> a
largestPrimeDiv x = divide x 2 1

divide :: (Integral a) => a -> a -> a -> a
divide x y latest = if y > x
						then latest
						else if x `mod` y == 0
								then divide (x `div` y) y y
								else divide x (y+1) latest

main = print $ largestPrimeDiv 600851475143