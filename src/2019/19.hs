module Day19 where

import Intcode

main = do
    program <- parseProgram <$> getContents
    let beam x y | x < 0 || y < 0 = False
                 | otherwise      = head (intcodeToList program [x, y]) == 1
    print . length $ [() | x <- [0..49], y <- [0..49], beam x y]
    let search y x | beam (x' + 99) (y - 99) = 10000 * x' + y - 99
                   | otherwise               = search (succ y) x'
                   where x' = head [x' | x' <- [x..], beam x' y]
    print (search 10 0)
