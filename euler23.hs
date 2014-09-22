import Control.Applicative
import Data.List

divisors :: Integer -> [Integer]
divisors n = filter ((==0) . rem n) [2.. n `div` 2]

isAbundant :: Integer -> Bool
isAbundant n = (sum $ divisors n) > n

abundants :: Integer -> [Integer]
abundants n = filter isAbundant [1..n]

sumOfAbundants :: Integer -> [Integer]
sumOfAbundants max = nub . sort . filter (<= max) $
--sumOfAbundants max = filter (<= max) $
       (+) <$> abundants max <*> abundants (max `div` 2)

notAbundants :: Integer -> [Integer]
notAbundants max = [1..max] \\ (sumOfAbundants max)

--main = print . sum $ notAbundants 2812
main = print $ (sum [1..max]) - (sum $ sumOfAbundants max)
        where max = 28123