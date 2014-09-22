import Data.List (isInfixOf)

t :: [Integer]
t = 1 : 1 : 1 : zipWith3 (\a b c -> a+b+c) t (tail t) (tail $ tail t)

restes p = map (`rem` p) t

--on sait qu'on a une boucle quand on trouve un couple de trois elements qu'on a deja vus
dansCycle :: [Integer] -> Bool
dansCycle u = ac u []
    where ac :: [Integer] -> [Integer] -> Bool
          ac v acc | head v == 0 = True
                   | hasBoucle v acc = False 
                   | otherwise           = ac (tail v) (head v : acc)

hasBoucle v acc = prep v `isInfixOf` acc
prep = reverse . take 3

main = print . (!! 26) $ filter (not . dansCycle . restes) [1,3..]
