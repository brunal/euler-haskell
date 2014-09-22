problem_9 sum = head [a*b*c | c <- [0..sum], b <- [0..c], a <- [0..b], a+b+c == sum, a^2+b^2 == c^2]

main = print $ problem_9 1000