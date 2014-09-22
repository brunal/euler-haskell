import System.Environment (getArgs)

--cette fonction ne marche pas.
--regarder du cote du theoreme d'euler
fakeHyper :: (Integral b) => Integer -> b -> Integer
fakeHyper a 1 = keep a
fakeHyper a b = keep $ a^(fakeHyper a (b-1))

keep :: (Integral a) => a -> a
keep = flip rem (10^8)

main = do
    args <- getArgs
    let [a,b] = map read args
    print $ fakeHyper a b
    --print $ fakeHyper 1777 1855