module Day05 where

import AOC

format = eachLine do
  [[x1, y1], [x2, y2]] <- number `sepBy` "," `sepBy` " -> "
  pure ((x1, y1), (x2, y2))

toPoints part2 ((x1, y1), (x2, y2))
  | x1 == x2 || y1 == y2 = [(x, y) | x <- fromTo x1 x2 , y <- fromTo y1 y2]
  | part2                = [(x, y) | x <- fromTo x1 x2 | y <- fromTo y1 y2]
  | otherwise            = []

main :: IO ()
main = do
  input <- parseInput format
  for_ [False, True] \part2 -> do
    print . howMany (> 1) . counts $ foldMap (toPoints part2) input
