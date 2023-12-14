module Day14 where

import AOC

import Data.Map qualified as M
import Data.Array qualified as A

gravity = go 0 where
  fill void = replicate void '.'
  go void [] = fill void
  go void ('#':xs) = fill void ++ '#':go 0 xs
  go void ('O':xs) = 'O':go void xs
  go void ('.':xs) = go (succ void) xs

main = do
  (input, width, height) <- makeGrid' <$> readInput
  let bounds = ((0, 0), (width-1, height-1))
      grid = A.array bounds (M.assocs input)
      tiltNorth grid = A.array bounds [((x, y), t) | x <- [0..width-1], (y, t) <- zip [0..] (gravity [grid A.! (x, y) | y <- [0..height-1]])]
      tiltSouth grid = A.array bounds [((x, height-1-y), t) | x <- [0..width-1], (y, t) <- zip [0..] (gravity [grid A.! (x, y) | y <- reverse [0..height-1]])]
      tiltWest grid = A.array bounds [((x, y), t) | y <- [0..height-1], (x, t) <- zip [0..] (gravity [grid A.! (x, y) | x <- [0..width-1]])]
      tiltEast grid = A.array bounds [((width-1-x, y), t) | y <- [0..height-1], (x, t) <- zip [0..] (gravity [grid A.! (x, y) | x <- reverse [0..width-1]])]
      cycle = tiltEast . tiltSouth . tiltWest . tiltNorth
      load grid = sum [height - y | ((_, y), 'O') <- A.assocs grid]
  print $ load $ tiltNorth grid
  print $ load $ nTimesCycle cycle 1000000000 grid
