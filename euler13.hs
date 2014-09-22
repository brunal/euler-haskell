import Data.Char

euler_13 length contents = print . take length $ show . sum $ map read $ lines contents

main = do
	contents <- readFile "numbers.txt"
	euler_13 10 contents