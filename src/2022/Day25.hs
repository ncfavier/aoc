module Day25 where

import AOC

snafuToInt = foldl' (\ n d -> 5 * n + digit d) 0 where
  digit '=' = -2
  digit '-' = -1
  digit '0' = 0
  digit '1' = 1
  digit '2' = 2

intToSnafu = map digit . reverse . go where
  go 0 = []
  go n = let d = clampMod (-2, 2) n in d:go ((n - d) `quot` 5)
  digit (-2) = '='
  digit (-1) = '-'
  digit 0 = '0'
  digit 1 = '1'
  digit 2 = '2'

main :: IO ()
main = do
  input <- lines <$> readInput
  putStrLn $ intToSnafu $ sum $ map snafuToInt input
