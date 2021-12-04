module Day04 where

import AOC

import Data.Array.IArray

format :: Parser ([Int], [Array (Int, Int) (Int, Bool)])
format = do
  ns <- number `sepBy` ","
  newline *> newline
  bs <- some (hspace *> number) `endBy` newline `sepBy` newline
  let size = length (head bs)
  pure (ns, fmap (,False) . array ((0, 0), (size - 1, size - 1)) . flattenWithCoords <$> bs)

mark n = fmap \case
  ((== n) -> True, False) -> (n, True)
  x                       -> x

wins b = or [all snd [b ! f (i, j) | i <- r] | j <- r, f <- [id, swap]]
  where
    ((lo, _), (hi, _)) = bounds b
    r = [lo..hi]

score b = sum [n | (n, False) <- toList b]

search [] _ = []
search _ [] = []
search (n:ns) bs = map ((n *) . score) winners ++ search ns rest
  where
    (winners, rest) = partition wins (mark n <$> bs)

main :: IO ()
main = do
  (ns, bs) <- parseInput format
  let scores = search ns bs
  print (head scores)
  print (last scores)
