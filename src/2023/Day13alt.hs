module Day13 where

import AOC

format = (some ((== '#') <$> oneOf ".#") `endBy` newline) `sepBy` newline

folds = go [] where
  go past [] = [(past, [])]
  go past (x:xs) = (past, x:xs):go (x:past) xs

reflection xs = Left <$> go xs <|> Right <$> go (transpose xs) where
  go xs = [length past | (past, future) <- folds xs, notNull past, notNull future, and (zipWith (==) past future)]

analyse xs = (clean, smudged) where
  clean = head $ reflection xs
  Just smudged = find (/= clean) $ reflection . peeks not =<< holesOf (traverse . traverse) xs

score (Left n) = n * 100
score (Right n) = n

main = do
  input <- parseInput format
  let (clean, smudged) = unzip $ map analyse input
  print $ sum $ map score clean
  print $ sum $ map score smudged
