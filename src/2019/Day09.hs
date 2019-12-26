module Day09 where

import AOC
import Intcode

main :: IO ()
main = do
    program <- parseProgram <$> readInput
    print $ head $ intcodeToList program [1]
    print $ head $ intcodeToList program [2]
