isLychrel :: Integer -> Bool
isLychrel = lych 1
--   where lych i n

lych :: Integer -> Integer -> Bool
lych i n
     | i == 51   = False
     | otherwise = let next = n + (read .reverse $ show n)
                     in isPalindrome next || lych (i+1) next

isPalindrome :: Integer -> Bool
isPalindrome n = ns == reverse ns
        where ns = show n

main = print . length $ filter (not . isLychrel) [1..10000]
