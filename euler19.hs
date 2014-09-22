import Data.List (elemIndex)

type Day = (String, Int, Int, Int)

main = print $ length $ filter (\(d, d_, _, _) -> d_ == 1 && d == "Sun") century

century = takeWhile notFin . dropWhile notDebut $ days ("Mon", 1, 1, 1900)

notFin   (_, d, m, y) = not $ y == 2000 && m == 12 && d == 31
notDebut (_, d, m, y) = not $ y == 1901 && m == 1 && d == 1

days :: Day -> [Day]
days start = start:days (nextDay start)

nextDay :: Day -> Day
nextDay (day, 31, 12, year) = (nd day, 1, 1, year+1)						-- next year
nextDay (day, day_, month, year)
	| (dom year !! (month-1)) == day_ = (nd day, 1, month+1, year)		-- next month
	| otherwise					   	 = (nd day, day_+1, month, year)	-- casual next day

dow = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
nd "Sun" = "Mon"
nd day = case elemIndex day dow of
			Just n -> dow !! (n+1)
			Nothing -> undefined

dom :: Int -> [Int]
dom y = [31, 28 + (leap y), 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	where leap y = if y `mod` 4 == 0
					then if y `mod` 100 == 0
							then if y `mod` 400 == 0
									then 1
									else 0
							else 1
					else 0