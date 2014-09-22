import Data.Char (digitToInt)
import Data.List
import Data.Function (on)

cubes = map (^3) [0..10000]
cubes' = map (sort . show) $ cubes
groupes =  filter ((==5) . length) . group . sort $ cubes'
Just index = elemIndex (head $ head groupes) cubes'

main = do
    print $ (fromIntegral index)^3