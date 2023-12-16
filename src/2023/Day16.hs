module Day16 where

import AOC

import Data.Map qualified as M
import Data.Set qualified as S

main = do
  (grid, width, height) <- makeGrid' <$> readInput
  let
    move d '.' = [d]
    move d '/' = maybeToList $ lookup d [(right, up), (up, right), (down, left), (left, down)]
    move d '\\' = maybeToList $ lookup d [(right, down), (down, right), (up, left), (left, up)]
    move d '-' | d `elem` [left, right] = [d]
               | otherwise = [left, right]
    move d '|' | d `elem` [up, down] = [d]
               | otherwise = [up, down]
    next (p, d) = [(n, d') | d' <- move d (grid M.! p), let n = p + d', n `M.member` grid]
    beam start = length $ S.fromList $ map (fst . fst) $ bfs next [start]
  print $ beam ((0, 0), right)
  print $ maximum $ map beam $
       [((0, i), right) | i <- [0..height-1]]
    ++ [((width-1, i), left) | i <- [0..height-1]]
    ++ [((i, 0), down) | i <- [0..width-1]]
    ++ [((i, height-1), up) | i <- [0..width-1]]
