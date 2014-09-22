main = print $ divisible 20

divisible n = lcm' [1..n] 1

lcm' [] acc = acc
lcm' (x:xs) acc = lcm' xs $ lcm x acc

-- soluce haskell.org
euler_5 = foldr1 lcm [1..20]