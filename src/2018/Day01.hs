module Day01 where

import AOC

main = do
    ns <- parseInputLines number
    print $ sum ns
    print $ firstDuplicate $ scanl (+) (0 :: Integer) $ cycle ns
