module Day21 where

import AOC
import Intcode

main :: IO ()
main = do
    program <- parseInputProgram
    let springWalk = unlines -- (¬A ∨ ¬B ∨ ¬C) ∧ D
            [ "NOT A J"
            , "NOT B T"
            , "OR T J"
            , "NOT C T"
            , "OR T J"
            , "AND D J"
            , "WALK"
            ]
    print . last $ intcodeToList program (asciiToIntegers springWalk)
    let springRun = unlines -- (¬A ∨ ¬B ∨ ¬C) ∧ D ∧ (E ∨ H)
            [ "NOT A J"
            , "NOT B T"
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
    print . last $ intcodeToList program (asciiToIntegers springRun)
