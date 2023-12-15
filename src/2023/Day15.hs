module Day15 where

import AOC

import Data.Array qualified as A

format = (many (noneOf ",\n") `sepBy` ",") <* newline

hash = foldl' (\n c -> ((n + ord c) * 17) `mod` 256) 0

data Op = Put Int | Remove

parseOp s = case break (not . isAlpha) s of
  (lbl, "-") -> (lbl, Remove)
  (lbl, '=':(read -> n)) -> (lbl, Put n)

main = do
  input <- parseInput format
  print $ sum $ map hash input
  let ops = map parseOp input
      doOp' (lbl, Put n) [] = [(lbl, n)]
      doOp' (lbl, Put n) (x@(lbl', _):xs)
        | lbl == lbl' = (lbl, n):xs
        | otherwise = x:doOp' (lbl, Put n) xs
      doOp' (lbl, Remove) l = filter ((/= lbl) . fst) l
      doOp arr (lbl, op) = arr & ix (hash lbl) %~ doOp' (lbl, op)
      score' l = sum [i * n | (i, (_, n)) <- zip [1..] l]
      score arr = sum [(b + 1) * score' l | (b, l) <- A.assocs arr]
  print $ score $ foldl doOp (A.listArray (0, 255) (repeat [])) ops
