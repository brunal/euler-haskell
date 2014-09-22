main = print $ diffSquareSum 100

diffSquareSum n = sum frame ^ 2 - (sum $ map (^2) frame)
	where frame = [1..n]