main = print . length $ lamina 1000000

lamina :: Int -> [Int]
lamina max = concat . takeWhile (not . (== [])) $ map (takeWhile (<=max)) laminas

laminas :: [[Int]]
laminas = map lam [3..]

lam :: Int -> [Int]
lam start = scanl1 (+) $ map outline [start,start+2..]
            where outline n = 4*(n-1)
