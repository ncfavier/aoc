module Day24 where

import AOC

import Data.Map qualified as M

main :: IO ()
main = do
  (grid, width, height) <- makeGrid' <$> readInput
  let start = (1, 0)
      end = (width - 2, height - 1)
      retro n d p = let (x, y) = p - n `mul` d in (clampMod (1, width - 2) x, clampMod (1, height - 2) y)
      noBlizzard n p = and [grid M.! retro n d p /= c | (d, c) <- zip [up, down, left, right] ['^', 'v', '<', '>']]
      allowed n p = p == start || p == end || (inRange ((1, 1), (width - 2, height - 2)) p && noBlizzard n p)
      heuristic p = manhattan (p - end)
      next (n, p) = [((succ n, p'), 1, heuristic p') | d <- origin:cardinal, let p' = p + d, allowed (succ n) p']
      search from to n = head [n | ((n, p), _) <- astar next [(n, from)], p == to]
      part1 = search start end 0
      part2 = search start end (search end start part1)
  print part1
  print part2
