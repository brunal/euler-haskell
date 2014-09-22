import Data.Char (intToDigit)

-- value points to spend
-- higest digit possible = max
possibilities :: Int -> Int -> [[Int]]
possibilities value maxi | value == 0 = [[]]
--possibilities value maxi | value == 1 = [[1]]
possibilities value maxi | maxi   == 1 = [replicate value 1]
possibilities value maxi = do
                    headDigit <- [1..min value maxi]
                    tailDigits <- possibilities (value - headDigit) headDigit
                    return (headDigit:tailDigits)

--main = print $ map (\x -> length $ possibilities x x) [2..30]
--main = print $ possibilities 7 7
--main = print . length $ possibilities 1000 1000


--u1 = 1
--u2 = 2
--u(n,p) = somme(i=1 a i=min(n,p) ; u(n-i,i))
--u(0,p) = 0
--u(1,p) = 1+u(0,1) = 1
--u(n,1) = 1+u(n-1,1) = ... = n

poss :: Int -> Int -> Int
poss value maxi | value == 0 = 0
--poss value maxi | value == 1 = 1
poss value maxi | maxi   == 1 = 1
poss value maxi = sum [poss (value - headDigit) headDigit | headDigit <- [1..min value maxi]]
--do
--                    headDigit <- [1..min value maxi]
--                    return (1 + poss (value - headDigit) headDigit)

main = print $ poss 7 7
--main = print $ map (\x -> poss x x) [2..30]