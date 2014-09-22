import Data.Char (intToDigit)

candidates :: [Integer]
candidates = map (10*) [10^8..142*10^6]

isOk :: Integer -> Bool
isOk n = "1234567890" == (oneOfTwo . show $ n^2)
    where oneOfTwo (x:_:xs) = x : oneOfTwo xs
          oneOfTwo xs       = xs
          
main = print . head $ filter isOk candidates