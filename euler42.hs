import Data.Char (ord)
import Data.List (sort)

triangle = map (\x -> x*(x+1) `div` 2) [1..]

position c = ord c - 64

parse :: String -> [String]
parse = map read . splitOn (==',')

value :: String -> Int
value = foldl (\acc c -> acc + (position c)) 0

main = do
     contents <- readFile "words.txt"
     print . length $ isIn (sort . map value $ parse contents) triangle

isIn :: [Int] -> [Int] -> [Int]
isIn [] _ = []
isIn (x:xs) (y:ys)
        | x < y  = isIn xs (y:ys)
        | x == y = x:isIn xs (y:ys) 
        | x > y  = isIn (x:xs) ys



splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f l@(x:xs)
  | f x = splitOn f xs
  | otherwise = let (h,t) = break f l in h:(splitOn f t)
