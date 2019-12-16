module Day9 where

import Intcode

main :: IO ()
main = do
    program <- parseProgram <$> getContents
    print . head =<< runIntcode program [1]
    print . head =<< runIntcode program [2]
