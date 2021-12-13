module Day13 where

import AOC

import Data.Set qualified as S

format = do
  grid <- ((,) <$> number <* "," <*> number) `endBy` newline
  newline
  folds <- ((,) <$ "fold along " <*> (first <$ "x" <|> second <$ "y") <* "=" <*> number) `endBy` newline
  pure (S.fromList grid, folds)

doFold (direction, n) = S.map $ direction (\i -> if i > n then 2 * n - i else i)

main :: IO ()
main = do
  (grid, folds@(f0:_)) <- parseInput format
  print $ length $ doFold f0 grid
  let folded = foldl (flip doFold) grid folds
      ((xmin, ymin), (xmax, ymax)) = boundingBox folded
  for_ [ymin..ymax] \y -> do
    putStrLn [if (x, y) `S.member` folded then '#' else ' ' | x <- [xmin..xmax]]
