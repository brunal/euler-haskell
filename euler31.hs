import Data.List (concat, inits)

englishCoins :: [Int]
englishCoins = [1, 2, 5, 10, 20, 50, 100, 200]

possibilities :: Int -> [Int] -> [[Int]]
possibilities 0 _ = [[]]
possibilities n coins = concat . map (poss n) . tail . inits $ takeWhile (<= n) coins
	where poss n coins' = map (last coins':) $ possibilities (n- (last coins')) coins'
	
main = print . length $ possibilities 200 englishCoins