main = print $ foldl (\(nacc,lacc) (n,l) -> if l > lacc
											then (n,l)
											else (nacc,lacc)) (0,0) $ sequenceLength [1..1000000] []
	
--main = print $ sequenceLength [1..100] []
	
sequenceLength :: [Int] -> [(Int, Int)] -> [(Int, Int)]
sequenceLength [] acc = acc
sequenceLength (x:xs) acc = sequenceLength xs  $ (x, seq x):acc
	where seq n
		| n == 1 = 1
		| even n = 1 + case lookup (n `div` 2) acc of
							Just p -> p
							Nothing -> seq (n `div` 2)
		| otherwise = 1 + case lookup (n*3+1) acc of
							Just p -> p
							Nothing -> seq (n*3+1)