module Day21 where

import AOC
import Intcode

main = do
    program <- parseProgram <$> getContents
    let springWalk = unlines
            [ "NOT A J"
            , "NOT B T"
            , "OR T J"
            , "NOT C T"
            , "OR T J"
            , "AND D J"
            , "WALK"
            ]
    print . last $ intcodeToList program (map (fromIntegral . ord) springWalk)
    let springRun = unlines
            [ "NOT B J"
            , "NOT A T"
            , "OR T J"
            , "NOT C T"
            , "OR T J"
            , "AND D J"
            , "NOT E T"
            , "NOT T T"
            , "OR H T"
            , "AND T J"
            , "RUN"
            ]
    print . last $ intcodeToList program (map (fromIntegral . ord) springRun)
