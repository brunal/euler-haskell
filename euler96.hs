import Data.Char (digitToInt)

type Puzzle = [[Int]]

parse :: String -> [Puzzle]
parse = reverse . map (parse' . reverse) . foldl (\acc l -> if (length $ head acc) == 10
                                        then [l]:acc
                                        else (l:(head acc)):(tail acc)
                           ) [[]] . lines

parse' :: [String] -> Puzzle
parse' = map (map digitToInt) . tail


solve :: Puzzle -> Puzzle
solve lss@(l:ls) = do
    



main = do
    file <- readFile "sudoku.txt"
    let puzzles = parse file
    let solved = map solve puzzles
    print . sum (concat . map show . take 3 . head) puzzles