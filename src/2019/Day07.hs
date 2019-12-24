module Day07 where

import Data.List

import Intcode

runLoop :: [Integer] -> [Integer] -> Integer
runLoop program [a, b, c, d, e] =
    let run  = intcodeToList program
        outA = run (a:0:outE)
        outB = run (b:outA)
        outC = run (c:outB)
        outD = run (d:outC)
        outE = run (e:outD)
    in last outE

main :: IO ()
main = do
    program <- parseProgram <$> getContents
    print $ maximum $ runLoop program <$> permutations [0..4]
    print $ maximum $ runLoop program <$> permutations [5..9]
