module Day25 where

import AOC
import Intcode

script :: String
script = unlines
    [ north
    , north
    , take "sand"
    , south
    , south
    , south
    , west
    , take "wreath"
    , south
    , south
    , take "pointer"
    , north
    , north
    , east
    , north
    , west
    , south
    , take "planetoid"
    , north
    , west
    , south
    , west
    , north
    ]
    where north  = "north"
          south  = "south"
          east   = "east"
          west   = "west"
          take o = "take " ++ o

main :: IO ()
main = do
    program <- parseProgram <$> readInput
    putStrLn $ filter isDigit
             $ last
             $ lines
             $ integersToAscii
             $ intcodeToList program
             $ asciiToIntegers script
