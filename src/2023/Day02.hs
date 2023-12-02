module Day02 where

import AOC

import Data.Map qualified as M

format :: Parser [(Int, [Map String Int])]
format = eachLine $ (,) <$ "Game " <*> number <* ": " <*> (set `sepBy` "; ")
  where
    set = M.fromList <$> (cubes `sepBy` ", ")
    cubes = flip (,) <$> number <* " " <*> word

base = M.fromList [("red", 12), ("green", 13), ("blue", 14)]

main :: IO ()
main = do
  input <- parseInput format
  print $ sum [i | (i, sets) <- input, all (M.isSubmapOfBy (<=) `flip` base) sets]
  print $ sum [product joins | (_, sets) <- input, let joins = foldr (M.unionWith max) (0 <$ base) sets]
