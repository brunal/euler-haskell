import qualified Data.Map as M

nextIndex :: Int -> Int
nextIndex n = if even n 
			then n `div` 2
			else n*3+1
			
populate :: Int -> M.Map Int Int
populate n = foldl populate' (M.singleton 1 1) [1..n]

populate' :: M.Map Int Int -> Int -> M.Map Int Int
populate' m n = case M.lookup n m of
				Just _ -> m												-- we already know the value, so we just pass the map
				Nothing -> let n' = nextIndex n							-- it is unknown, let's compute it
							in let m' = populate' m n'					-- recursion (terminal case: n=1 which we know and will always happen here)
							in M.insert n (1 + case M.lookup n' m' of	-- we insert it at 1 further than its child
												Just p' -> p'			-- we *have* to check for that
												Nothing -> undefined) m'-- this line should never be triggered

main = print $ findMax $ populate 300000

findMax :: M.Map Int Int -> (Int, Int)
findMax = foldl (\(nacc,lacc) (n,l) -> if l > lacc
										then (n,l)
										else (nacc,lacc)) (0,0) . M.toList