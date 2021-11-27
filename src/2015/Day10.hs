module Day10 where

import AOC

describe = concatMap (\(c, n) -> show n ++ [c]) . rle

main :: IO ()
main = do
  [n] <- lines <$> readInput
  let part1 = (describe `nTimes` 40) n
  print (length part1)
  let part2 = (describe `nTimes` 10) part1
  print (length part2)
