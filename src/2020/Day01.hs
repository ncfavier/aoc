module Day01 where

import AOC

solve :: Int -> [Int] -> Int
solve n nums = product . head $ go n nums [] where
    go 0 _ xs = xs <$ guard (sum xs == 2020)
    go n nums xs = do
        x:tail <- tails nums
        go (pred n) tail (x:xs)

main = do
    nums <- parseInputLines number
    print (solve 2 nums)
    print (solve 3 nums)
