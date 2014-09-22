import Data.List

d :: Int -> Int
d = sum . divisors

divisors :: Int -> [Int]
divisors n = 1 : divi n 2

divi :: Int -> Int -> [Int]
divi n p | p^2 == n = [p]
divi n p | p^2 > n = []
divi n p = if n `mod` p == 0
			then p:(n `div` p):(divi n (p+1))
			else divi n (p+1)
			
			
genDiv :: Int -> [(Int, Int)]
genDiv n = foldl (\acc n -> (n, d n):acc) [] [1..n]

amicable :: [(Int, Int)] -> [Int]
amicable [] = []
amicable ((x,s):xs) = case lookup s xs of
						Just t -> if t == x
										then x:s:amicable (delete (s,t) xs)
										else amicable xs
						Nothing -> amicable xs

main = print $ sum . amicable $ genDiv 9999