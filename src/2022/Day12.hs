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
  let start = head [p | (p, t) <- M.assocs grid, t == 'S']
      end = head [p | (p, t) <- M.assocs grid, t == 'E']
      grid' = height <$> grid
      next p = [n | n <- (p +) <$> cardinal, Just hn <- [grid' M.!? n], hp `reaches` hn]
        where hp = grid' M.! p
      prev n = [p | p <- (n +) <$> cardinal, Just hp <- [grid' M.!? p], hp `reaches` hn]
        where hn = grid' M.! n
  print $ head [d | (p, d) <- bfs next start, p == end]
  print $ head [d | (p, d) <- bfs prev end, grid' M.! p == height 'a']
