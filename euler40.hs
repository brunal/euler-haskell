import Data.Char (digitToInt)

number :: String
number = fuse $ map show [1..]

fuse :: [String] -> String
fuse (x:xs) = x ++ fuse xs

main = print . product $ map (\n -> digitToInt $ number !! (10^n-1)) [0..6]