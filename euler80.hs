import Data.List ((\\))

digits :: Integer -> [Integer]
digits 0 = []
digits n = let (q,r) = quotRem n 10
            in r : digits q

maxi = 100
sujets = [1..maxi] \\ (map (^2) [1..intsqrt maxi])
main = print . sum $ map (sum . digits . intsqrt . shift) sujets
        where shift = (*) . (^2) . (10^) $ maxi-1

intsqrt n = head . dropWhile (\x -> x*x > n) $ iterate (\x -> (x + n `div` x) `div` 2) (n `div` 2)
