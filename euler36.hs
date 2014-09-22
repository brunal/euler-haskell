import Data.Char (chr)
import Numeric (showIntAtBase)

isPalindrome :: String -> Bool
isPalindrome n = n == reverse n

isOk :: Int -> Bool
isOk n = (isPalindrome $ show n) && (isPalindrome $ base2 n)

base2 :: Int -> String
base2 n = showIntAtBase 2 (chr . (+48)) n ""

main = print . sum $ filter isOk [1..999999]