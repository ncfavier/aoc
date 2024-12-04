module Day04 where

import AOC

import Data.Map qualified as M

format = takeRest

main = do
  input <- makeGrid <$> readInput
  let lookup i = M.findWithDefault '.' i input
  print $ length [() | start <- M.keys input, d <- principal, let word = lookup . (start +) . (*d) <$> [0, 1, 2, 3], word == "XMAS"]
  print $ length [() | start <- M.keys input, lookup start == 'A', and [sort [lookup (start + d), lookup (start - d)] == "MS" | d <- [(1, 1), (-1, 1)]]]
