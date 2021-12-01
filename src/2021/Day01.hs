module Day01 where

import AOC

main :: IO ()
main = do
  measurements <- parseInput (eachLine number)
  for_ [1, 3] \n -> do
    print $ howMany (uncurry (<)) $ zip measurements (drop n measurements)
