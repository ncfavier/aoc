module Day05 where

import AOC
import Intcode

main :: IO ()
main = do
    program <- parseProgram <$> readInput
    print $ last $ intcodeToList program [1]
    print $ last $ intcodeToList program [5]
