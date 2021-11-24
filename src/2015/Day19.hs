module Day19 where

import Data.Map qualified as Map
import Data.Set qualified as Set
import System.IO

import AOC

type Atom = String
type Molecule = [Atom]

format :: Parser (Atom ~> [Molecule], Molecule)
format = (,) . groups <$> rule `endBy` newline <* newline <*> some atom <* newline
  where
    atom = (:) <$> upperChar <*> many lowerChar
    rule = (,) <$> word <* " => " <*> some atom

alter :: Atom ~> [Molecule] -> Molecule -> [Molecule]
alter rules m = do
  (xs, y:ys) <- zip (inits m) (tails m)
  zs <- concat (rules Map.!? y)
  pure $ xs ++ zs ++ ys

main :: IO ()
main = do
  (rules, start) <- parseInput format
  print $ length $ Set.fromList $ alter rules start
  hFlush stdout
  print $ head [n | (m, n) <- bfs (alter rules) ["e"], m == start] -- runs out of memory
