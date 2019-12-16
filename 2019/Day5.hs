module Day5 where

import Intcode

main :: IO ()
main = do
    program <- parseProgram <$> getContents
    print . last =<< runIntcode program [1]
    print . last =<< runIntcode program [5]
