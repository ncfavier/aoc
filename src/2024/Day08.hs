module Day08 where

import AOC

import Data.Map qualified as M
import Data.Set qualified as S

main = do
  grid <- makeGrid <$> readInput
  let
    antennas = M.fromListWith (<>) [(a, [p]) | (p, a) <- M.assocs grid, a /= '.']
    antinodes l = [b + b - a | (a, l') <- pickOne l, b <- l']
    antinodes' l = [p | (a, l') <- pickOne l, b <- l', let d = b - a, p <- takeWhile (`M.member` grid) (iterate (+d) b)]
  print $ length $ S.filter (`M.member` grid) $ S.fromList $ foldMap antinodes antennas
  print $ length $ S.filter (`M.member` grid) $ S.fromList $ foldMap antinodes' antennas
