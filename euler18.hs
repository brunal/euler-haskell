main = do
        contents <- readFile "triangle.txt" --"euler18.txt"
        print $ triangle $ parse contents

parse :: String -> [[Int]]
parse = map ((map read) . words) . lines

triangle :: [[Int]] -> Int
triangle =  maximum . foldl1 (\xs acc -> zipWith (+) acc $ zipWith max (xs ++ [0]) (0:xs))
