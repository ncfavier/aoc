module Day09 where

import AOC

format = eachLine do many (lexeme number)

d = zipWith subtract <*> tail

extrapolate x
  | all (== 0) x = 0
  | otherwise = last x + extrapolate (d x)

main = do
  report <- parseInput format
  print $ sum $ map extrapolate report
  print $ sum $ map (extrapolate . reverse) report
