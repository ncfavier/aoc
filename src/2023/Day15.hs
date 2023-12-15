module Day15 where

import AOC

import Data.Array qualified as A

format = (many (noneOf ",\n") `sepBy` ",") <* newline

hash = foldl' (\n c -> ((n + ord c) * 17) `mod` 256) 0

operation label "-" lenses = filter ((/= label) . fst) lenses
operation label ('=':(read -> n)) lenses
  | lens:_ <- holesOf @(->) (traversed.itraversed.index label) lenses = peek n lens
  | otherwise = lenses ++ [(label, n)]

step arr (break (not . isAlpha) -> (label, op)) = arr & ix (hash label) %~ operation label op

power = getSum . view ((itraversed <.> itraversed <. traversed) . withIndex . to (\((i, j), l) -> Sum (succ i * succ j * l)))

main = do
  input <- parseInput format
  print $ sum $ map hash input
  print $ power $ foldl step (A.listArray (0, 255) (repeat [])) input
