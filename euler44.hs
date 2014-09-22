pentagonals :: [Integer]
pentagonals = map pent [1..]

pent :: Integer -> Integer
pent n = n*(3*n-1) `quot` 2

elem' :: Integer -> [Integer] -> Bool
elem' n (x:xs) = case n `compare` x of
					GT -> n `elem'` xs
					EQ -> True
					LT -> False
	
good :: (Integer, Integer) -> Bool
good (r,j) = (pentk `elem'` pentagonals) && ((pentk + (pent j)) `elem'` pentagonals)
	where pentk = pent r + pent j

main = print . head $ filter good candidates

candidates :: [(Integer, Integer)]
candidates = iterate nextCandidate (1,1)
--(1,1) : fmap nextCandidate candidates

nextCandidate :: (Integer, Integer) -> (Integer, Integer)
nextCandidate (r,j) | (pent (j+1)) - (pent j) > (pent r) = (r+1,1)
nextCandidate (r,j) = (r,j+1)