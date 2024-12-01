module Day23 where

import AOC

import Data.Map qualified as M
import Data.Set qualified as S

-- WIP

tileToDir '>' = Just right
tileToDir '<' = Just left
tileToDir '^' = Just up
tileToDir 'v' = Just down
tileToDir _ = Nothing

main = do
  (grid, width, height) <- makeGrid' <$> readInput
  let
    start = head [p | (p@(x, 0), v) <- M.assocs grid, v == '.']
    neighbours dry p | not dry, Just d <- tileToDir (grid M.! p) = [p + d]
                     | otherwise = [p' | p' <- (p +) <$> cardinal, Just t <- [grid M.!? p'], t /= '#']
    nexts dry = M.mapWithKey go grid where
      go p t = go' 1 p <$> neighbours dry p
      go' n p p' | [p''] <- delete p (neighbours dry p') = go' (n + 1) p' p''
               | otherwise = (p', n)
    next dry (p, ps) = [((p', S.insert p ps), c) | (p', c) <- nd M.! p, not (p' `S.member` ps)]
      where nd = nexts dry
  mapM_ print $ [(d, ps) | (((x, y), ps), d) <- dijkstraOn id (next True) [(start, S.empty)], y == height-1]
