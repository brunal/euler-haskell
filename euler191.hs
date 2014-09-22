import Data.List (isPrefixOf)
import Control.Applicative ((<*>), (<$>))

recoitunprix :: String -> Bool
recoitunprix = (&&) <$> not . toolate <*> not . tooaway
toolate (x:xs) = x == 'L' && 'L' `elem` xs
tooaway = isPrefixOf "AAA"

sequences :: Int -> [String]
sequences 0 = [""]
sequences n = [tt:qq | tt <- jour, qq <- sequences (n-1), recoitunprix (tt:qq)]
    where jour = "ALO"

main = print . length $ sequences 24