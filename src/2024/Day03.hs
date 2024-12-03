module Day03 where

import AOC

import Text.Regex.TDFA

main = do
  input <- readInput
  let matches = input =~ "do\\(\\)|don't\\(\\)|mul\\(([0-9]{1,3}),([0-9]{1,3})\\)" :: [[String]]
  print $ sum [read a * read b | ['m':_, a, b] <- matches]
  let go _ [] = []
      go _ (("do()":_):ms) = go True ms
      go _ (("don't()":_):ms) = go False ms
      go f (['m':_, a, b]:ms) = (if f then (read a * read b :) else id) (go f ms)
  print $ sum $ go True matches
