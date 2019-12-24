module Day09 where

import Intcode

main :: IO ()
main = do
    program <- parseProgram <$> getContents
    print $ head $ intcodeToList program [1]
    print $ head $ intcodeToList program [2]
