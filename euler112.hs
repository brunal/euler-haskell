import Data.List (sort) --, inits)
--import Data.Char (intToDigit)

--increasing :: [Int]
--increasing = concat $ map (inc 1) [1..]
--    where inc min 1   = [min..9]
--          inc min len = do
--                a <- [min..9]
--                b <- inc a (len-1)
--                return . read $ show a ++ show b

--decreasing :: [Int]
--decreasing = do
--    len <- [1..]
--    max <- [1..9]
--    decr <- dec max len
--    return . read $ intToDigit max : decr
--    where dec :: Int -> Int -> [String]
--          dec max 1   = map show [0..max]
--          dec max len = do
--                a <- [0..max]
--                b <- dec a (len-1)
--                return $ intToDigit a : b

--bouncy :: [Int]
--bouncy = [1..] `minus` increasing `minus` decreasing
--    where minus (x:xs) (y:ys) = case x `compare` y of
--                                    LT -> x:(minus xs (y:ys))
--                                    EQ -> minus xs ys
--                                    GT -> minus (x:xs) ys


isBouncy x = (xx /= sort xx) && (reverse xx /= sort xx)
    where xx = show x

main = print . prop 0.99 $ filter isBouncy [1..]
    where prop p = snd . head . filter (\(a,b) -> a >= p * fromIntegral b) . zip [1..]


--main = print . last . head . dropWhile (\ls -> 100*(length ls) `quot` (last ls) < 90) . tail . inits $ bouncy