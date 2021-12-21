module Day21 where

import AOC
import Data.Map.Strict qualified as M

format :: Parser (Int, Int)
format = (,) <$ "Player 1 starting position: " <*> decimal <* newline
             <* "Player 2 starting position: " <*> decimal <* newline

part1 :: Int -> Int -> Int
part1 = go det 0 0 0
  where
    det = sum <$> chunksOf 3 (cycle [1..100])
    go (d:ds) !n !s1 !s2 !p1 !p2
      | s2 >= 1000 = s1 * n
      | otherwise = go ds (n + 3) s2 (s1 + p1') p2 p1'
      where p1' = (p1 + d) `mod1` 10

part2 :: Int -> Int -> Int
part2 = maximum . M.fromListWith (+) .* go 1 True 0 0
  where
    nondet = M.assocs . counts $ sum <$> replicateM 3 [1..3]
    go !c !p !s1 !s2 !p1 !p2
      | s2 >= 21 = pure (p, c)
      | otherwise = do
          (d, c') <- nondet
          let p1' = (p1 + d) `mod1` 10
          go (c * c') (not p) s2 (s1 + p1') p2 p1'

main :: IO ()
main = do
  (p1, p2) <- parseInput format
  print $ part1 p1 p2
  print $ part2 p1 p2
