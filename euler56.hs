import Data.Char

digitalSum :: Integer -> Int
digitalSum = sum . map digitToInt . show

main = print . maximum $ map digitalSum [a^b | a <- [1..100], b <- [1..100]]
