import Data.Char
import Data.List

main = do
	contents <- readFile "names.txt"
	print $ sum' . compute $ parse contents
	
parse :: String -> [String]
parse = sort . map read . splitOn (==',')

compute :: [String] -> [Int]
compute [] = []
compute (str:s) = (sum $ map (\c -> ord c - 64) str):compute s

sum' :: [Int] -> Int
sum' xs = sum $ zipWith (*) xs [1..(length xs)]


splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f l@(x:xs)
  | f x = splitOn f xs
  | otherwise = let (h,t) = break f l in h:(splitOn f t)