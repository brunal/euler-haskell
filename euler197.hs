f :: Double -> Double
f x = (floor $ 2^(30.403243784-x^2))/(10^9)

u :: [Double]
u = iterate f (-1.0)

main = print $ (u !! (10^12)) + (u !! (10^12+1))