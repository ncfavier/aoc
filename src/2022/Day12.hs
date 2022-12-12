module Day12 where

import AOC

import Data.Map qualified as M

height 'S' = height 'a'
height 'E' = height 'z'
height c = ord c - ord 'a'

a `reaches` b = b - a <= 1

main :: IO ()
main = do
  grid <- makeGrid <$> readInput
  let start = [p | (p, t) <- M.assocs grid, t == 'S']
      grid' = height <$> grid
      lowest = [p | (p, t) <- M.assocs grid', t == height 'a']
      next p = [n | n <- (p +) <$> cardinal, Just hn <- [grid' M.!? n], hp `reaches` hn]
        where hp = grid' M.! p
  print $ head [d | (p, d) <- bfs next start, grid M.! p == 'E']
  print $ head [d | (p, d) <- bfs next lowest, grid M.! p == 'E']
