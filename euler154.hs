import Monad (guard)

coefficientsOk :: Integer -> [Integer]
coefficientsOk n = do
        k <- [1..n]
        p <- [1..k]
        let coef = ((product [n-k+1..n]) `quot` (product [1..k]))*((product [k-p+1..k]) `quot` (product [1..p]))
        guard $ coef `rem` 10^12 == 0
        return coef
        
main = print . length $ coefficientsOk 200000