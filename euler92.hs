import Data.Char (digitToInt)

next :: Int -> Int
next = foldl (\acc n -> acc + ((^2) $ digitToInt n)) 0 . show
--next = sum . map ((^2) . digitToInt) . show

ourLoop :: Int -> Bool
ourLoop n = case next n of
            89 -> True
            1  -> False
            n' -> ourLoop n'
            
main = print . length . filter ourLoop $ [1..10000000]