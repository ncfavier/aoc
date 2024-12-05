module Day05 where

import AOC

format = do
  order <- many ((,) <$> number <* "|" <*> number <* newline)
  newline
  updates <- many (number `sepBy` "," <* newline)
  pure (order, updates)

main = do
  (order, updates) <- parseInput format
  let
    cmp x y | (x, y) `elem` order = LT
            | (y, x) `elem` order = GT
            | otherwise = EQ
    ordered u = sortBy cmp u == u
  print $ sum [u !! (length u `div` 2) | u <- updates, ordered u]
  print $ sum [u' !! (length u' `div` 2) | u <- updates, let u' = sortBy cmp u, u /= u']
