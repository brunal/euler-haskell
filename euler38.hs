import Data.List ((\\))

main = print $ maximum pantadigitals

pantadigitals :: [Int]
pantadigitals = map read . filter isPanta $ map pantaProduct [1..12345]

pantaProduct :: Int -> String
pantaProduct n = last . takeWhile ((<=9) . length) . scanl (++) [] $ map (show . (*n)) [1..9]

isPanta :: String -> Bool
isPanta n = length n == 9 && n \\ "123456789" == []