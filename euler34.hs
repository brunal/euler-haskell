import Data.Char

isCurious :: Int -> Bool
isCurious 1 = False
isCurious 2 = False
isCurious n = n == (sum . map (\c -> product [1..digitToInt c]) $ show n)

main = print . sum $ isCurious [1..1000000]