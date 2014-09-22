parmi k n = product [n-k+1..n] `div` (product [1..k])

tousLesParmi = [k `parmi` n | n <- [1..100], k <- [1..n]]

main = print . length $ filter (>= 1000000) tousLesParmi