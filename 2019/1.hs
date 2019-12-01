fuel x = x `div` 3 - 2

main = do
    ns <- map read . lines <$> readFile "input1"
    print $ sum $ map fuel ns
    print $ sum $ map (sum . takeWhile (> 0) . tail . iterate fuel) ns
