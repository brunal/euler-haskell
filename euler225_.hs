import Data.List (tails)

t :: [Integer]
t = 1 : 1 : 1 : zipWith3 (\a b c -> a+b+c) t (tail t) (tail $ tail t)

restes p = map (`rem` p) t

--on sait qu'on a une boucle quand on trouve un couple de trois elements qu'on a deja vus
avantCycle :: [Integer] -> [Integer]
avantCycle u = ac (triplets u) []
    where ac :: [[Integer]] -> [[Integer]] -> [Integer]
          ac v acc | head v `elem` acc = reverse $ map head acc
                   | otherwise         = ac (tail v) (head v : acc)

triplets :: [Integer] -> [[Integer]]
triplets u = zipWith3 (\x y z -> z:y:x:[]) u (tail u) (tail $ tail u)

isDivisibleBy = elem 0 . avantCycle . restes

main = print $ filter (not . isDivisibleBy) [1,3..]
