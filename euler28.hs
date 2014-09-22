sumDiag grid = sum $ 1 : takeNjump (tail grid) 1 1 4

takeNjump :: [Int] -> Int -> Int -> Int -> [Int]
takeNjump [] _ _ _ = []
takeNjump grid _ nMax 0 = let nMax' = nMax + 2
                          in takeNjump grid nMax' nMax' 4
takeNjump grid 0 nMax p = head grid : takeNjump (tail grid) nMax nMax (p-1)
takeNjump grid n nMax p  = takeNjump (tail grid) (n-1) nMax p


main = print $ sumDiag [1..1001^2]
