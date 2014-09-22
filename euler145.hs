import Data.Char (digitToInt)
import System.Environment (getArgs)

isReversible :: Int -> Bool
isReversible n = noTrailingZeroes n && (onlyOdd $ n + rev n)
    where rev = read . reverse . show
          onlyOdd = all odd . map digitToInt . show
          noTrailingZeroes x = not $ x `rem` 10 == 0

main = do
    foo <- getArgs
    let max = read $ head foo
    print . length $ filter isReversible [1..10^max]
    return ()