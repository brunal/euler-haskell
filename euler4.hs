main = print $ largestPalindrome 3

largestPalindrome n = let frame = reverse [10^(n-1)..10^n-1]
						in palindromeLookUp frame frame 0

palindromeLookUp [] _ pal = pal
palindromeLookUp (_:xs) [] pal = palindromeLookUp xs xs pal
palindromeLookUp xss@(x:xs) (y:ys) pal = if isPalindrome try && try > pal
										then palindromeLookUp xss ys try
										else palindromeLookUp xss ys pal
											where try = x*y

isPalindrome n = nStr == reverse nStr
	where nStr = show n
	
	
-- soluce haskell.org
problem_4 = maximum [ x | y <- [100..999],
                          z <- [y..999],
                          let x = y * z,
                          let s = show x,
                          s == reverse s ]