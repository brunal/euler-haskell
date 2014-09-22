import Math.Sieve.Phi
import Data.List
import Data.Function (on)

limit :: Int
limit = 10000000

s = sieve limit

main = print . mini . filter isPermutation $ map (\x -> (x, phi s x)) [2..limit]

isPermutation (x, y) = (sort . show $ x) == (sort . show $ y)

--mini = minimumBy (compare `on` (\(x, y) -> x/y)) 
mini = minimumBy (\(x,px) (y,py) -> compare (x*py) (y*px))
