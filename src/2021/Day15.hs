module Day15 where

import AOC

import Data.Map qualified as M

plus :: Int -> Int -> Int
a `plus` b = (a + b - 1) `mod` 9 + 1

solve :: Map (Int, Int) Int -> Int -> Int -> Int
solve grid width height = fromJust $ lookup (width - 1, height - 1) (dijkstra step (0, 0))
  where step = withNeighbours cardinal (,) grid

main :: IO ()
main = do
  (fmap digitToInt -> grid, width, height) <- makeGrid <$> readInput
  let grid5 = M.unions [ plus (i + j) <$> M.mapKeysMonotonic (+ offset) grid
                       | (i, j) <- range ((0, 0), (4, 4))
                       , let offset = (width * i, height * j)]
  print $ solve grid width height
  print $ solve grid5 (width * 5) (height * 5)
