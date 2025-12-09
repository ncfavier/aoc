module Day09 where

import AOC

format = eachLine do
  (,) <$> decimal <* "," <*> decimal

area ((x1, y1), (x2, y2)) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

main = do
  input <- parseInput format
  print $ maximum $ area <$> pairs input
