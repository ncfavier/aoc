module Day11 where

import AOC

import Data.Set qualified as S

expandX n = S.fromAscList . go 0 . S.toAscList
  where
    go _ [] = []
    go i ((x, y):gs) | i == x = (x, y):go i gs
                     | i < x = go (x + dx) (map (_1 +~ dx) ((x, y):gs))
                     where dx = (n - 1)*(x - i - 1)

expand n = transposeGrid . expandX n . transposeGrid . expandX n

main = do
  grid <- mapToSet (== '#') . makeGrid <$> readInput
  for [2, 1000000] \ n ->
    print $ sum [manhattan (a - b) | (toList -> [a, b], _) <- pickSubset 2 (expand n grid)]
