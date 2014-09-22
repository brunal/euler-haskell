import System.IO (readFile)
import Data.List (intercalate, isPrefixOf)

replacements :: [([Char], [Char])]
replacements = [ ("IIIII", "V")
               , ("IIII", "IV")
               , ("VIV", "IX")
               , ("VV", "X")
               , ("XXXXX", "L")
               , ("XXXX", "XL")
               , ("LXL", "XC")
               , ("LL", "C")
               , ("CCCCC", "D")
               , ("CCCC", "CD")
               , ("DCD", "CM")
               , ("DD", "M")
               ]
-- what / with what / where
replace :: String -> String -> String -> String
replace s t = intercalate t . splitOn s

-- what / where
splitOn :: String -> String -> [String]
splitOn s txt = let (prefix, txt') = breakwhere s txt
                in prefix : case txt' of
                                [] -> []
                                _  -> splitOn s txt'

breakwhere :: String -> String -> (String, String)
breakwhere = bw []
    where bw accu what [] = (reverse accu, [])
          bw accu what txt | what `isPrefixOf` txt = (reverse accu, drop (length what) txt)
                           | otherwise             = bw ((head txt):accu) what (tail txt)

better :: [Char] -> [Char]
better roman = better' roman replacements
        where better' rom [] = rom
              better' rom repl@((old,new):rep) = better' (replace old new rom) rep

saved :: [Char] -> Int
saved roman = length roman - length (better roman)

main = do
    content <- readFile "roman.txt"
    print . sum . map (saved . (++".")) $ lines content