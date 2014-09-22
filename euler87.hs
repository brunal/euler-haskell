import qualified Data.Set as Set

maximal :: Int -> Int -> Int
maximal limit power = floor $ fromIntegral limit ** (1/fromIntegral power)

combinaisons :: Int -> [Int]
combinaisons limit = do
                   a <- magic 4 limit
                   b <- magic 3 $ limit - a
                   c <- magic 2 $ limit - a - b
                   return $ a+b+c
             where magic y z = map (^y) $ takeWhile (<= (maximal z y)) primesTME


main = print . length . nubOrd $ combinaisons 50000000


nubOrd :: Ord e => [e] -> [e] 
nubOrd xs = go Set.empty xs where
  go s (x:xs)
   | x `Set.member` s = go s xs
   | otherwise        = x : go (Set.insert x s) xs
  go _ _              = []

primesTME = 2 : gaps 3 (join [[p*p,p*p+2*p..] | p <- primes'])
  where
    primes' = 3 : gaps 5 (join [[p*p,p*p+2*p..] | p <- primes'])
    join  ((x:xs):t)        = x : union xs (join (pairs t))
    pairs ((x:xs):ys:t)     = (x : union xs ys) : pairs t
    gaps k xs@(x:t) | k==x  = gaps (k+2) t 
                    | True  = k : gaps (k+2) xs

union (x:xs) (y:ys) = case (compare x y) of 
           LT -> x : union  xs  (y:ys)
           EQ -> x : union  xs     ys 
           GT -> y : union (x:xs)  ys