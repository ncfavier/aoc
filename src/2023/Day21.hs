module Day21 where

import AOC

import Data.Map qualified as M
import Data.Set qualified as S

main = do
  grid <- makeGrid <$> readInput
  let
    start = head [p | (p, 'S') <- M.assocs grid]
    neighbours = M.mapWithKey (\p _ -> S.fromList [n | d <- cardinal, let n = p + d, Just t' <- [grid M.!? n], t' /= '#']) grid
    step ps = S.unions (S.map (neighbours M.!) ps)
  print $ length $ nTimes step 64 (S.singleton start)
