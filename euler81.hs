main = do
    file <- readFile "matrix.txt"
    print $ problem_82 file

problem_82 = minSum . map parse . lines

parse :: String -> [Int]
parse = read . ('[':) . (++ "]")

minSum :: [[Int]] -> Int
minSum (x:xs) = last $ (foldl nextLine) (scanl1 (+) x) xs

nextLine :: [Int] -> [Int] -> [Int]
nextLine (p:pl) (n:nl) = scanl nextCell (p+n) (zip pl nl)
    where nextCell acc (prev, new) = new + min prev acc