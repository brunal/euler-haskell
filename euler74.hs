-- A surveiller :
-- 145
-- 169 (+2)
-- 871 (+1)
-- 872 (+1)
-- 1454 (+2)
-- 45361 (+1)
-- 45362 (+1)
-- 363601 (+2)

main = print . length $ filter ((==60) . uniqLength) [1..1000000]

uniqLength n = ul [] $ iterate next n
    where ul xs (y:ys) = if y `elem` xs
                            then 0
                            else 1 + ul (y:xs) ys

next = sum . map fac . composantes
            where fac n = product [1..n]

composantes 0 = []
composantes n = let (a,b) = quotRem n 10
                in b : composantes a