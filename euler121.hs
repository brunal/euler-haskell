main = print . (1/) $ proba 15

--probabilité de gagner quand n tours
proba :: Integer -> Double
proba n =  (fromIntegral $ proba' n) / (product [1..fromIntegral n+1])

--nombre de parties où sont tirés plus de bleus que de rouges
proba' n = sum $ map (blue 1 n) [n `quot` 2 + 1..n]

--nombre de parties où sont tirés k disques bleus sur les n, en commençant avec r disques rouges
blue :: Integer -> Integer -> Integer -> Integer
blue _ 0 0 = 1
blue r n 0 = r*(blue (r+1) (n-1) 0)
blue _ n k | k > n = 0
--           | k == n = 1 --facultatif
blue r n k = (blue (r+1) (n-1) (k-1)) + r*(blue (r+1) (n-1) k)
