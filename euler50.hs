import Data.List (concat, unzip, maximumBy)
import Data.Function (on)
import Monad (guard)

--8 minutes...
--main = print . maximumBy (compare `on` snd) . concat $ map (onlyPrimes . sums) listsToSum
--    where listsToSum :: [[Int]]
--          listsToSum = takeWhile ((<1000) . head) $ iterate tail primes     -- at least 20 primes summed in the value we're looking for => primes until 1,000,000/20 max -- fact even less are needed
--          sums :: [Int] -> [(Int,Int)]     -- (value, numberOrPrimesInTheSum)
--          sums = takeWhile ((<=1000000) . fst) . scanl (\(val,num) prime -> (val+prime, num+1)) (0,0)       -- receives an infinite list of consecutive primes, sums (1, then 1-2, then 1-3, ...) it until one sum is bigger than 10^6
--          onlyPrimes :: [(Int, Int)] -> [(Int, Int)]
--          onlyPrimes = strangeIntersect primes

strangeIntersect :: (Ord a) => [a] -> [(a, b)] -> [(a, b)]
strangeIntersect (x:xs) ((y@(val,_)):ys) = case x `compare` val of
                            LT -> strangeIntersect xs (y:ys)
                            EQ -> y : strangeIntersect xs ys
                            GT -> strangeIntersect (x:xs) ys
strangeIntersect _       []              = []
                            
primes :: [Int]
primes = 2 : sieve [3,5..]  where
    sieve []     = []
    sieve (p:xs) = p : sieve (xs `minus` [p*p,p*p+2*p..])

minus (x:xs) (y:ys) = case (compare x y) of
          LT -> x : minus  xs  (y:ys)
          EQ ->     minus  xs     ys
          GT ->     minus (x:xs)  ys
minus  xs     _     = xs



--Slower + wrong result? -_-
main = print . maximumBy (compare `on` snd) $ do
    let primes'' = takeWhile (<100000) primes
    let primes' = takeWhile (<1000) primes
    start <- [0..length primes' - 1]
    length <- [1..length primes' - start]
    let sum' = sum . take length $ drop start primes'
    guard $ sum' < 1000000
    guard $ sum' `elem` primes''
    return (sum', length)