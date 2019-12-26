module Day02 where

import AOC

main = do
    ids <- lines <$> readInput
    let cs = map counts ids
        c n = count (any (== n)) cs
    print $ c 2 * c 3
    putStrLn $ head [d | (x, xs) <- pickOne (transpose ids), d <- findDuplicates (transpose xs)]
