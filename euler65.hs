import Data.List (foldr)
import Data.Ratio
import Data.Char (digitToInt)

continued :: [Integer]
continued = 2:1:inter [1,1] [2,4..]
    where inter x (y:ys) = y:x ++ inter x ys

convergent :: [Integer] -> Int -> Ratio Integer
convergent continued n = foldr reduct (last cont % 1) (init cont)
    where reduct new accu = (new*(numerator accu) + denominator accu) % numerator accu
          cont = take n continued
    
main = print . sum . map digitToInt . show . numerator $ convergent continued 100