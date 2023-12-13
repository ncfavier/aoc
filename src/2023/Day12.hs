module Day12 where

import AOC

format = eachLine do (,) <$> takeWhileP (/= ' ') <* space1 <*> number `sepBy` ","

arrangements = memoFix \ go -> \case
  ('?':xs, ns) -> go ('.':xs, ns) + go ('#':xs, ns)
  ('.':xs, ns) -> go (xs, ns)
  (xs@('#':_), (n:ns))
    | length xs == n && all (/= '.') (take n xs) -> go (drop n xs, ns)
    | length xs > n && all (/= '.') (take n xs) && (xs !! n) /= '#' -> go (drop (n + 1) xs, ns)
    | otherwise -> 0
  ('#':_, []) -> 0
  ([], []) -> 1
  ([], _) -> 0

unfold (xs, ns) = (intercalate "?" (replicate 5 xs), concat (replicate 5 ns))

main = do
  input <- parseInput format
  print $ sum $ map arrangements input
  print $ sum $ map (arrangements . unfold) input
