module Day24 where

import AOC

import Data.Map qualified as M
import Data.Set qualified as S

foo (a, b) n = a + (n - a) `mod` (b - a)

main :: IO ()
main = do
  (grid, width, height) <- makeGrid' <$> readInput
  let start = (1, 0)
      end = (width - 2, height - 1)
      retro n d p = let (x, y) = p - n `mul` d in (foo (1, width - 1) x, foo (1, height - 1) y)
      safe n p = p == start || p == end || (p `M.member` grid && grid M.! p /= '#' && and [grid M.! retro n d p /= c | (d, c) <- zip [up, down, left, right] ['^', 'v', '<', '>']])
      heuristic p = manhattan (p - end)
      next (n, p) = [((succ n, p'), 1, heuristic p') | d <- origin:cardinal, let p' = p + d, safe (succ n) p']
      search = astar next [(0, start)]
      yay = head [n | ((n, p), _) <- search, p == end]
      search2 = astar next [(yay, end)]
      yay2 = head [n | ((n, p), _) <- search2, p == start]
      search3 = astar next [(yay2, start)]
      yay3 = head [n | ((n, p), _) <- search3, p == end]
  print yay
  print yay3
