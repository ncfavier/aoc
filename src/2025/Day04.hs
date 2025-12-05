module Day04 where

import AOC

main = do
  rolls <- mapToSet (== '@') . makeGrid <$> readInput
  let
    step = evolve (neighbours ((0,0):principal)) (\ (Cell b i) -> b && i >= 4)
    final = fixedPointOn length step
    diff f = length rolls - length (f rolls)
  print (diff step)
  print (diff final)
