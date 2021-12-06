module Day06 where

import AOC

import Data.Map qualified as M

format :: Parser [Int]
format = number `sepBy` "," <* newline

grow :: Map Int Integer -> Map Int Integer
grow m = M.insertWith (+) 6 n . M.insertWith (+) 8 n . M.mapKeysMonotonic pred . M.delete 0 $ m
  where n = fromMaybe 0 $ m M.!? 0

main :: IO ()
main = do
  fish <- counts <$> parseInput format
  let history = iterate grow fish
  for_ [80, 256] \i -> do
    print . sum $ history !! i
