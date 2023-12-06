module Day05 where

import AOC

format = do
  seeds <- "seeds: " *> many (hlexeme number) <* newline
  newline
  maps <- map `sepBy` newline
  pure (seeds, maps)
  where
    map = do
      (from, to) <- (,) <$> word <* "-to-" <*> word <* " map:" <* newline
      ranges <- ((,,) <$> hlexeme number <*> hlexeme number <*> hlexeme number) `endBy` newline
      pure (from, to, ranges)

mapFun (_, _, ranges) = Endo \n -> head $ [a + n - b | (a, b, r) <- ranges, inRange (b, b+r-1) n] <|> [n]

main :: IO ()
main = do
  (seeds, maps) <- parseInput format
  print $ minimum $ map (appEndo (foldMap mapFun (reverse maps))) seeds
