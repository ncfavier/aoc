module Day05 where

import Data.Interval as I
import Data.IntervalSet as IS

import AOC

format = do
  seeds <- "seeds: " *> many (hlexeme number) <* newline
  newline
  maps <- map `sepBy` newline
  pure (seeds, maps)
  where
    map = do
      (,) <$> word <* "-to-" <*> word <* " map:" <* newline
      ranges <- ((,,) <$> hlexeme number <*> hlexeme number <*> hlexeme number) `endBy` newline
      pure ranges

mapRange is (a, b, r) = (fromInteger (a - b) + (is `IS.intersection` IS.singleton i), IS.delete i is)
  where i = Finite b <=..< Finite (b + r)
mapRanges ranges = Endo \is -> let (new, old) = foldlM mapRange is ranges in new <> old

lowest is = case lowerBound (IS.span is) of Finite n -> n

main :: IO ()
main = do
  (seeds, maps) <- parseInput format
  let is = IS.fromList (I.singleton <$> seeds)
  print $ lowest $ appEndo (foldMap mapRanges (reverse maps)) is
  let is = IS.fromList [Finite a <=..< Finite (a + r) | [a, r] <- chunksOf 2 seeds]
  print $ lowest $ appEndo (foldMap mapRanges (reverse maps)) is
