import Data.List (transpose)

main = do
    file <- readFile "matrix.txt"
    print $ problem_82 file

problem_82 = minSum . transpose . map parse . lines

parse :: String -> [Int]
parse = read . ('[':) . (++ "]")

-- from list of lines to list of colums
minSum :: [[Int]] -> Int
minSum = minimum . foldl1 nextLine

nextLine :: [Int] -> [Int] -> [Int]
nextLine a b = zipWith min (nextLine' a b) . reverse $ nextLine' (reverse a) (reverse b)
nextLine' (p:pl) (n:nl) = scanl nextCell (p+n) (zip pl nl)
    where nextCell acc (prev, new) = new + min prev acc