import Data.List (intersperse)

sequence :: (Int, [Int])
sequence = (2, 1 : intersperse 1 [2,4..])

fraction :: [Int] -> Rational
fraction (x:xs) = 1 % (x + fraction xs)

convergent n = let (r,xs) = take n sequence
                in a + fraction $ take n xs
                
main = length . show $ convergent 100