import Data.List
import Data.Char (digitToInt)
import Data.Function (on)
import Data.Monoid (mappend)
import IO (readFile)

type Hand = ([Int], [Char])  -- each of them: length = 5

data Score = HC Int | OP Int | TP Int Int | ToaK Int | S | F | FH Int | FoaK Int | SF | RF   deriving (Read, Show, Eq, Ord)

value :: Hand -> Score
value (values, colors) =
    let flush = and . map (== head colors) $ tail colors
        straight = replicate 4 1 == zipWith (\x y -> x-y) values (tail values)
        byValues = group values                                 -- groups cards by their face value,  type [[Card]]
        same = map length $ byValues                            -- returns the size of each group,    type [Int]
        bestSame = maximum same                                 -- biggest number of identical cards, type Int
        rankOfBestSame = head $ maximumBy (compare `on` length) byValues -- face value of the card of the group that has the biggest number of identical cards, type Int
        
    in if flush && straight
        then if head values == 13
              then RF
              else SF
        else if bestSame == 4
              then FoaK rankOfBestSame
              else if sort same == [2,3]
                    then FH rankOfBestSame
                    else if flush
                          then F
                          else if straight
                                then S
                                else if bestSame == 3
                                      then ToaK rankOfBestSame
                                      else if sort same == [1,2,2]
                                            then let r2 = head . head . tail $ sortBy (compare `on` length) byValues
                                            in TP rankOfBestSame r2
                                            else if bestSame == 2
                                                  then OP rankOfBestSame
                                                  else HC (head values)

parse :: String -> (Hand, Hand)
parse = (\(h1, h2) -> let mix = unzip . reverse . sortBy (compare `on` fst) in (mix h1, mix h2)). splitAt 5 . map (\(x:y:[]) -> (charToValue x, y)) . words
    where charToValue :: Char -> Int
          charToValue c = case c of
                            'A' -> 14
                            'K' -> 13
                            'Q' -> 12
                            'J' -> 11
                            'T' -> 10
                            otherwise -> digitToInt c                      

main = do
    text <- readFile "poker.txt"
    let hands = map parse $ lines text
    return . length . filter (==GT) $ map (\(h1,h2) -> compare (value h1) (value h2) `mappend` compare (fst h1) (fst h2)) hands