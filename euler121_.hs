import Data.Ratio

main = print . (\r -> denominator r `quot` numerator r) $ proba 15

--probabilité de gagner quand n tours
proba :: Integer -> Ratio Integer
proba n = sum $ map (blue 1 n) [n `quot` 2 + 1..n]

--probabilité d'avoir k disques bleus sur les n, en commençant avec r disques rouges
blue :: Integer -> Integer -> Integer -> Ratio Integer
blue _ _ 0 = 1
blue _ n k | k > n = 0
blue r n k = let ab = (blue (r+1) (n-1) (k-1)) + (fromIntegral r*(blue (r+1) (n-1) k))
                in numerator ab % (denominator ab * (r+1))
