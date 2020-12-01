module Day01 where

import AOC

main = do
    Just ns <- parseMaybe (linesOf number) <$> readInput
    print $ sum ns
    print $ firstDuplicate $ scanl (+) (0 :: Integer) $ cycle ns
