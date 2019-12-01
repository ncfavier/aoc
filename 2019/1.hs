fuel x = (x `div` 3) - 2

main = do
    part1 <- sum . map (fuel . read) . lines <$> readFile "input1"
    print part1
    part2 <- sum . map (sum . takeWhile (> 0) . tail . iterate fuel . read) . lines <$> readFile "input1"
    print part2
