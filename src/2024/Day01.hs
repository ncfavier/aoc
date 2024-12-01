module Day01 where

import AOC

import Data.Map qualified as M

format = eachLine do (,) <$> lexeme number <*> number

main = do
  (a, b) <- unzip <$> parseInput format
  print $ sum $ zipWith dist (sort a) (sort b)
  let c = counts b
  print $ sum [i * M.findWithDefault 0 i c | i <- a]
