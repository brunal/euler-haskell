main = do
	contents <- readFile "grid.txt"
	let best = (\x -> getBestOf $ products 4 (parse contents) x)
	print $ maximum [best horiz, best verti, best diago, best diago']
	
parse :: String -> [[Int]]
parse contents = map (map read . words) $ lines contents

-- For each cell we need to look at 3 directions: right, bottom and diagonally
products :: Int -> [[Int]] -> ([[Int]] -> [[Int]]) -> [[Int]]
products 1 grid cut = grid
products size grid cut = multiplyGrids grid $ products (size-1) (cut grid) cut

verti :: ([[Int]] -> [[Int]])
verti = tail

horiz :: ([[Int]] -> [[Int]])
horiz = map tail

diago :: ([[Int]] -> [[Int]])
diago grid = map tail $ tail grid

diago' :: ([[Int]] -> [[Int]])
diago' grid = map (0:) $ tail grid


getBestOf :: [[Int]] -> Int
getBestOf grid = maximum $ map maximum grid

multiplyGrids :: [[Int]] -> [[Int]] -> [[Int]]
multiplyGrids [] _ = []
multiplyGrids _ [] = []
multiplyGrids (x:xm) (y:ym) = multiplyLists x y : multiplyGrids xm ym

multiplyLists :: [Int] -> [Int] -> [Int]
multiplyLists [] _ = []
multiplyLists _ [] = []
multiplyLists (x:xs) (y:ys) = x*y : multiplyLists xs ys