module Day02 where

import Intcode

run program noun verb = mem ! 0
    where Halt mem = runIntcode (head program:noun:verb:drop 3 program)

main :: IO ()
main = do
    program <- parseProgram <$> getContents
    print $ run program 12 2
    print $ head [ 100 * noun + verb
                 | noun <- [0..99]
                 , verb <- [0..99]
                 , run program noun verb == 19690720
                 ]
