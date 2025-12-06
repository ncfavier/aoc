module Day06 where

import AOC

format = do
  (,) <$> many (many (hlexeme decimal) <* eol) <*> many (hlexeme printChar) <* eol

operation '+' = sum
operation '*' = product

main = do
  (numbers, ops) <- parseInput format
  print $ sum $ zipWith operation ops (transpose numbers)
