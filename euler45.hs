triangle = map (\n -> n*(n+1) `div` 2) [1..]		:: [Integer]
pentagonal = map (\n -> n*(3*n-1) `div` 2) [1..]	:: [Integer]
hexagonal = map (\n -> n*(2*n-1)) [1..]		:: [Integer]


inter :: [Integer] -> [Integer] -> [Integer]
inter (x:xs) (y:ys)
	| x == y = x:inter xs ys
	| x > y  = inter (x:xs) ys
	| x < y  = inter xs (y:ys)
inter [] _ = undefined
inter _ [] = undefined
	
inter' :: [Integer] -> [Integer] -> [Integer] -> [Integer]
inter' a b c = inter a $ inter b c
						   
main = print $ (inter' triangle pentagonal hexagonal) !! 2