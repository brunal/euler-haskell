proba :: (Fractional a) => Int -> Int -> Int -> a
proba n 0 0 = 1
proba n 0 _ = 0
proba n p x = sum [proba n (p-1) (x-i) | i <- [1..n]] / (fromIntegral n)

main = print . sum $ map proba1wins [1..9*4]
    where proba1wins thisRoll = d1 thisRoll * (sum $ map d2 [1..thisRoll-1])
          d1 = proba 4 9
          d2 = proba 6 6