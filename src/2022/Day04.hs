module Day04 where

import AOC

format = eachLine $ (,) <$> range <* "," <*> range
  where range = (,) <$> number <* "-" <*> number

rangeLeq :: Ord a => (a, a) -> (a, a) -> Bool
(a, b) `rangeLeq` (c, d) = a >= c && b <= d

rangeComparable :: Ord a => (a, a) -> (a, a) -> Bool
rangeComparable a b = a `rangeLeq` b || b `rangeLeq` a

rangeOverlap :: Ord a => (a, a) -> (a, a) -> Bool
rangeOverlap (a, b) (c, d) = not $ a > d || b < c

main :: IO ()
main = do
  ranges <- parseInput format
  print $ howMany (uncurry rangeComparable) ranges
  print $ howMany (uncurry rangeOverlap) ranges
