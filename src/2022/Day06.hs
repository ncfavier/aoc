module Day06 where

import AOC

format = many letterChar <* newline

firstUniqueChunk :: Eq a => Int -> [a] -> Maybe Int
firstUniqueChunk n stream = fmap (+ n) $ findIndex (\chunk -> nub chunk == chunk) $ divvy n 1 stream

main :: IO ()
main = do
  buffer <- parseInput format
  print $ fromJust $ firstUniqueChunk 4 buffer
  print $ fromJust $ firstUniqueChunk 14 buffer
