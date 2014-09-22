ok :: Int -> [Integer]
ok n = filter ((==n) . length . show) $ map (^n) [1..9]

main = print . sum $ map (length . ok) [1..25]