import Data.Bits (xor)
import Data.Char (ord, chr)
import Data.List (cycle, isPrefixOf, tails)

keys = [ [a,b,c] | a <- lettres, b <- lettres, c <- lettres ]
    where lettres = map ord ['a'..'z']
    
decrypt :: [Int] -> [Int] -> [Int]
decrypt message key = zipWith xor message $ cycle key

findKey :: ([Int] -> [Int]) -> [Int]
findKey toDecrypt = head . filter isEnglish $ map toDecrypt keys

isEnglish :: [Int] -> Bool
isEnglish message = all (\n -> n >= 32 && n <= 122) message && isEnglish' message
isEnglish' = or . map (isPrefixOf $ map ord "and") . tails

main = do
    text <- readFile "cipher1.txt"
    let message = read $ '[' : text ++ "]"
    let clearMessage = findKey $ decrypt message
    print $ map chr clearMessage
    print . sum . findKey $ decrypt message