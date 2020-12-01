module Day09 where

import AOC
import Intcode

main :: IO ()
main = do
    program <- parseInputProgram
    print $ head $ intcodeToList program [1]
    print $ head $ intcodeToList program [2]
