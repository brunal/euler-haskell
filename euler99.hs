import Data.List (zip, maximumBy)
import Data.Function (on)

main = do
    file <- readFile "base_exp.txt"
    putStrLn . show . findMax $ parse file
    return ()
    
parse :: String -> [(Float, Float)]
parse = map (\line -> let (b,e) = break (','==) line 
                        in (read b, read $ tail e)
            ) . lines

findMax :: [(Float, Float)] -> Int
findMax = fst . maximumBy (compare `on` snd) . zip [1..] . map (\(b,e) -> e*(log b))     --(\(b,e) -> b^e)