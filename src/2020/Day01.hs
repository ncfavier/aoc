module Day01 where

import AOC

solve n nums = head $ go n nums []
    where
        go 0 _ xs = do
            guard (sum xs == 2020)
            pure (product xs)
        go n nums xs = do
            x:tail <- tails nums
            go (pred n) tail (x:xs)

main = do
    nums <- parseInputLines number
    print (solve 2 nums)
    print (solve 3 nums)
