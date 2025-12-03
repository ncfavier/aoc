module Day03 where

import AOC

import Data.List.Extra (dropEnd)

format = eachLine (many digit)

joltage = go 0 where
  go acc 0 _ = acc
  go acc n l = go (10 * acc + i) (n - 1) l' where
    i = maximum (dropEnd (n - 1) l)
    l' = tail $ dropWhile (/= i) l

main = do
  batteries <- parseInput format
  print $ sum $ map (joltage 2) batteries
  print $ sum $ map (joltage 12) batteries
