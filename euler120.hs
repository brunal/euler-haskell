import Data.List (foldl')

r a n = ((a-1)^n + (a+1)^n) `rem` a^2
rmax a = maximum $ map (r a) [1..a*2]   --*2: [1..a] was giving a wrong result

main = print $ foldl' (\acc new -> rmax new + acc) 0 [3..1000]