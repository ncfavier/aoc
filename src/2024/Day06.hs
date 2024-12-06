module Day06 where

import AOC

import Data.Map qualified as M
import Data.Set qualified as S

main = do
  grid <- makeGrid <$> readInput
  let
    [start] = [p | (p, '^') <- M.assocs grid]
    step grid pos dir
      | M.lookup (pos + dir) grid == Just '#' = step grid pos (cw dir)
      | otherwise = (pos, dir) : step grid (pos + dir) dir
    walk grid = takeWhile ((`M.member` grid) . fst) $ step grid start up
  print $ length $ S.fromList $ map fst $ walk grid
  let
    loops p = notNull $ findDuplicates $ walk (M.insert p '#' grid)
  print $ howMany loops [p | (p, '.') <- M.assocs grid]
