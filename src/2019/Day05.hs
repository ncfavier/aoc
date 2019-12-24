module Day05 where

import Intcode

main :: IO ()
main = do
    program <- parseProgram <$> getContents
    print $ last $ intcodeToList program [1]
    print $ last $ intcodeToList program [5]
