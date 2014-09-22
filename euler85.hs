import Data.Function (on)
import Data.List (minimumBy)

rect n p = n*(n+1)*p*(p+1) `div` 4

main = print $ minimumBy (compare `on` (abs . ((-) 2000000)) . snd) rects
        where rects = filter ((>1999900) . snd) $ filter ((<2000100) . snd) [(n*p, rect n p) | n <- [1..53], p <- [n..2000]]