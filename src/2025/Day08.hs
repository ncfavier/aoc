module Day08 where

import AOC

import Data.Map qualified as M

format = eachLine do
  (,,) <$> decimal <* "," <*> decimal <* "," <*> decimal

replace from to x | x == from = to
                  | otherwise = x

-- dumbest union-find ever
step reps (from, to) = replace (reps M.! from) (reps M.! to) <$> reps

main = do
  boxes <- parseInput format
  let
    connections = sortOn (uncurry euclideanDist3) (pairs boxes)
    steps = scanl' step (M.fromList [(a, a) | a <- boxes]) connections
    answer (Just (((a, _, _), (b, _, _)), _)) = a * b
  print $ product $ take 3 $ sortDesc $ M.elems $ counts $ steps !! 1000
  print $ answer $ find (allEqual . M.elems . snd) $ zip connections (tail steps)
