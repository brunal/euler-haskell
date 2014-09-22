main = print . sum $ map (length . word) [1..1000]

word :: Int -> String
word n = if n < 20
           then lookup' n w1
           else if n < 100
                  then (lookup' (n - (n `mod` 10)) w2) ++ (word $ n `mod` 10)
                  else if not $ n == 1000
                        then (lookup' (n `div` 100) w1) ++ w3 ++ if n `mod` 100 == 0
                                                                   then ""
                                                                   else w_ ++ (word $ n `mod` 100)
                        else (lookup' 1 w1) ++ w4



w1 :: [(Int, String)]
w1 = zip [0..19] ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

w2 :: [(Int, String)]
w2 = zip [20,30..90] ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

w3 = "hundred"
w4 = "thousand"
w_ = "and"

lookup' :: Int -> [(Int, String)] -> String
lookup' n list = case lookup n list of
                Just s  -> s
                Nothing -> undefined
