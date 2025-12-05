module Day05 where

import AOC

format = do
  let range = (,) <$> decimal <* "-" <*> decimal
  (,) <$> many (range <* eol) <* eol <*> many (decimal <* eol)

main = do
  (ranges, ingredients) <- parseInput format
  print $ howMany (\ i -> any (\ r -> inRange r i) ranges) ingredients
  print $ intervalSetCardinality $ foldMap rangeToIntervalSet ranges
