import Data.List (permutations)

panta :: [String]
panta = permutations ['0'..'9']

targets :: String -> [Int]
targets (_:xs) = targets' xs
    where targets' (x:(xs@(y:z:_))) = (read [x,y,z]):(targets' xs)
          targets' _ = []

test :: String -> Bool
test = and . zipWith (\a b -> b `mod` a == 0) primes . targets

main = print . sum . map read $ filter test panta


primes :: [Int]
primes = 2 : sieve [3,5..]  where
    sieve []     = []
    sieve (p:xs) = p : sieve (xs `minus` [p*p,p*p+2*p..])

minus (x:xs) (y:ys) = case (compare x y) of
          LT -> x : minus  xs  (y:ys)
          EQ ->     minus  xs     ys
          GT ->     minus (x:xs)  ys
minus  xs     _     = xs