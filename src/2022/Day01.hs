module Day01 where

import AOC
import Data.Map qualified as M
import Data.Set qualified as S

format = many (number <* newline) `sepBy` newline

main :: IO ()
main = do
  elves <- reverse . sort . map sum <$> parseInput format
  print $ head elves
  print $ sum $ take 3 $ elves
