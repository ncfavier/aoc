module Day01 where

import AOC

format = eachLine do
  sign <- negate <$ "L" <|> id <$ "R"
  sign <$> decimal

howManyClicks = howMany (`divides` 100) . scanl (+) 50

main = do
  rotations <- parseInput format
  print $ howManyClicks rotations
  print $ howManyClicks $ (replicate <$> abs <*> signum) =<< rotations
