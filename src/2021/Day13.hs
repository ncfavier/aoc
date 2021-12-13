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
  (grid, f0:_) <- parseInput format
  print $ length $ doFold f0 grid
  let folded = S.map (f 40 *** f 6) grid
      f n i = if even (i `div` (n + 1)) then r else n - 1 - r
        where r = i `mod` (n + 1)
      ((xmin, ymin), (xmax, ymax)) = boundingBox folded
  for_ [ymin..ymax] \y -> do
    putStrLn [if (x, y) `S.member` folded then '#' else ' ' | x <- [xmin..xmax]]
