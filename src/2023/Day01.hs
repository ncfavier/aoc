module Day01 where

import AOC

format = eachLine takeRest

main :: IO ()
main = do
  input <- sum . map (read . sequence [head, last] .  (filter isDigit)) <$> parseInput format
  print input
