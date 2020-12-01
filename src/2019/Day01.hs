module Day01 where

import AOC

fuel x = x `div` 3 - 2

main = do
    ns <- parseInputLines number
    print $ sum $ map fuel ns
    print $ sum $ map (sum . takeWhile (> 0) . tail . iterate fuel) ns
