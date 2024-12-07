module Day07 where

import AOC

format = eachLine do
  (,) <$> number <* lexeme ":" <*> many (lexeme number)

values ops (n:ns) = go n ns where
  go acc [] = pure acc
  go acc (n:ns) = do
    op <- ops
    go (acc `op` n) ns

con n m = read (show n <> show m)

main = do
  input <- parseInput format
  print $ sum [test | (test, ns) <- input, test `elem` values [(+), (*)] ns]
  print $ sum [test | (test, ns) <- input, test `elem` values [(+), (*), con] ns]
