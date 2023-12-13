module Day13 where

import AOC

format = (some ((== '#') <$> oneOf ".#") `endBy` newline) `sepBy` newline

folds = go [] where
  go past [] = [(past, [])]
  go past (x:xs) = (past, x:xs):go (x:past) xs

reflection xs = head $ map (100 *) (go xs) ++ go (transpose xs) where
  go xs = [length past | (past, future) <- folds xs, notNull past, notNull future, and (zipWith (==) past future)]

smudge xs = head $ map (100 *) (go xs) ++ go (transpose xs) where
  go xs = [length past | (past, future) <- folds xs, notNull past, notNull future, howMany id (concat (zipWith (zipWith xor) past future)) == 1]

main = do
  input <- parseInput format
  print $ sum $ map reflection input
  print $ sum $ map smudge input
