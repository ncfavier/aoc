module Day14 where

import AOC

import Data.Map qualified as M

format = do
  start <- word <* newline <* newline
  let pair = (,) <$> letterChar <*> letterChar
  rules <- ((,) <$> pair <* " -> " <*> letterChar) `endBy` newline
  pure (start, M.fromList rules)

step rules (atoms, pairs) = (atoms', pairs')
  where
    atoms' = M.unionWith (+) atoms $ M.fromListWith (+)
      [(r, n) | ((a, b), n) <- M.toList pairs, let r = rules M.! (a, b)]
    pairs' = M.fromListWith (+) $ concat
      [[((a, r), n), ((r, b), n)] | ((a, b), n) <- M.toList pairs, let r = rules M.! (a, b)]

main :: IO ()
main = do
  (start, rules) <- parseInput format
  let atoms = counts start
      pairs = counts (zip start (tail start))
      steps = iterate (step rules) (atoms, pairs)
  for_ [10, 40] \n -> do
    let (atoms, _) = steps !! n
    print $ maximum atoms - minimum atoms
