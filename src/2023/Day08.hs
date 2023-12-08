module Day08 where

import AOC

import Data.Map qualified as M

format = do
  dirs <- many dir <* newline <* newline
  network <- M.fromList <$> eachLine do
    (,) <$> node <* " = (" <*> ((,) <$> node <* ", " <*> node) <* ")"
  pure (dirs, network)
  where
    node = many alphaNumChar
    dir = fst <$ char 'L' <|> snd <$ char 'R'

main = do
  (dirs, network) <- parseInput format
  let step node dir = dir (network M.! node)
      steps start end = length $ takeWhile (not . end) $ scanl step start (cycle dirs)
  print $ steps "AAA" (== "ZZZ")
  let endsWith c s = last s == c
  print $ foldl1 lcm $ map (steps `flip` endsWith 'Z') $ filter (endsWith 'A') (M.keys network)
