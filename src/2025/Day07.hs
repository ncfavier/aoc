module Day07 where

import AOC

import Data.Set qualified as S

split p '^' = (S.fromList [p-1, p+1], Sum 1)
split p _ = (S.singleton p, Sum 0)

p1 (tachyons, splits) row = (tachyons', splits + d)
  where (tachyons', Sum d) = foldMap (split <*> (row !!)) tachyons

p2 row worlds =
  [sumOf (folded.to (worlds !!)) (fst (split i c)) | (i, c) <- zip [0..] row]

main = do
  first:rows <- lines <$> readInput
  let Just start = elemIndex 'S' first
  print $ snd $ foldl' p1 (S.singleton start, 0) rows
  print $ foldr p2 (replicate (length first) 1) rows !! start
