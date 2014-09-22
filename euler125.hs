consecutivesquares :: [[Integer]]
consecutivesquares = map conssqu [1..]
            where conssqu debut = tail . scanl1 (+) $ map (^2) [debut..]

palindromic :: Integer -> Bool
palindromic n = let s = show n
                in s == reverse s

validnumbers :: Integer -> [[Integer]]
validnumbers maxi = map valid . take limit $ consecutivesquares
        where valid = filter palindromic . takeWhile (<maxi)
              limit  = (ceiling . sqrt . fromIntegral) maxi `div` 2

main = print . sum . map sum . validnumbers $ 10^8