module Day01 where

import AOC

format = many (number <* newline) `sepBy` newline

main :: IO ()
main = do
  elves <- reverse . sort . map sum <$> parseInput format
  print $ head elves
  print $ sum $ take 3 $ elves
