import Monad (guard)

pentagonals :: [Integer]
pentagonals = map (\n -> n*(3*n-1) `quot` 2) [1..]

elem' :: Integer -> [Integer] -> Bool
elem' n (x:xs) = case n `compare` x of
					GT -> n `elem'` xs
					EQ -> True
					LT -> False

main = print . head $ do
            a <- pentagonals
            b <- takeWhile (<a) pentagonals
            guard $ (a-b) `elem'` pentagonals
            guard $ (a+b) `elem'` pentagonals
            return (a-b)