import Data.Ratio
import Data.List (union, foldl1')
import System.Environment (getArgs)

fractions :: Int -> [Ratio Int]
fractions d = map (% d) [d `quot` 3 + 1..d `quot` 2 - if d `rem` 2 == 0  then 1 else 0]

problem73 :: Int -> Int
problem73 max = length . foldl1' union $ map fractions [1..max]

main = do
    args <- getArgs
    let max = read $ head args
    print $ problem73 max