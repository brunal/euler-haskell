import Monad (guard)
import Data.List (permutations, nub)

pantadigitals :: [Int]
pantadigitals = do
    perm <- permutations "123456789"    -- [[Char]]
    temp <- [1..length perm - 2]        -- [Int]
    let (a, remains) = splitAt temp perm   -- ([Char], [Char])
    
    temp' <- [1..length remains - 1]
    let (b, c) = splitAt temp' remains
    
    guard ((read a) * (read b) == (read c))
    
    return (read c)

main = print . sum . nub $ pantadigitals