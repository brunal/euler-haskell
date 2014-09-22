import Data.List (elemIndex, maximumBy)
import Data.Ord (comparing)

-- division "comme a la main"
-- quand on tombe sur un reste nul, la division est finie : nombre de décimales fini, pas de cycle
-- quand on tombe sur un reste qu'on a déja eu, c'est qu'on arrive a un endroit ou on etait deja... les decimales vont se repeter -> fini
-- on commence avec r=10 et pas r=1 : le resultat est le meme, mais ca supprime la premiere iteration (correspondant a un resultat en 0,... dans la division manuelle)
findCycle :: Int -> Int
findCycle d = restes d 1 []
	where restes d 0 rs = 0
	      restes d r rs = let r' = r `mod` d
                          in case elemIndex r' rs of
                               Just i  -> i+1
                               Nothing -> restes d (10*r') (r':rs)

main = print $ maximumBy (comparing snd) $ zip range (map findCycle range)
	where range = [1..999] :: [Int]