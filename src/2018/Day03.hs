module Day03 where

import AOC

format = eachLine claim
  where
    claim = (,,) <$ "#" <*> number <* " @ " <*> origin <* ": " <*> dimensions
    origin = (,) <$> number <* "," <*> number
    dimensions = (,) <$> number <* "x" <*> number

main :: IO ()
main = do
  claims <- parseInput format
  let grid = groups do
        (i, (x, y), (w, h)) <- claims
        p <- rectangle (x, y) (w, h)
        pure (p, i)
  print $ howMany ((>= 2) . length) grid
