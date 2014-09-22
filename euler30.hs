import Data.Char

digits :: Int -> [Int]
digits = map digitToInt . show

isOk :: Int -> Bool
isOk n = n == (sum . map (^5) $ digits n)


main = print . sum $ filter isOk [2..1000000]
