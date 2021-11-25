module Day01 where

import AOC

solve :: [Int] -> Int -> Int
solve nums n = product . head $ go nums n [] where
    go _    0 xs = xs <$ guard (sum xs == 2020)
    go nums n xs = do
        x:tail <- tails nums
        go tail (pred n) (x:xs)

main = do
    nums <- parseInput $ eachLine number
    for [2, 3] do solve nums >>> print
