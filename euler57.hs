ratios :: [(Integer, Integer)]
ratios = (3,2) : (map (\(n,d) -> (2*d+n,d+n)) ratios)

main = print . length . filter (\(n,d) -> (length $ show n) > (length $ show d)) $ take 1000 ratios