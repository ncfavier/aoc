module Day19 where

import Data.Map qualified as Map
import Data.Set qualified as Set

import AOC

type Atom = String
type Molecule = [Atom]

format :: Parser ([(Molecule, Molecule)], Molecule)
format = (,) <$> rule `endBy` newline <* newline <*> some atom <* newline
  where
    atom = (:) <$> upperChar <*> many lowerChar
    rule = (,) . pure <$> word <* " => " <*> some atom

alter :: Molecule `Map` [Molecule] -> Molecule -> [Molecule]
alter rules m = do
  (old, news) <- Map.assocs rules
  (prefix, tail) <- zip (inits m) (tails m)
  Just suffix <- pure $ stripPrefix old tail
  new <- news
  pure $ prefix ++ new ++ suffix

main :: IO ()
main = do
  (rules, medicine) <- parseInput format
  print $ length $ Set.fromList $ alter (groups rules) medicine
  print $ head [n | (m, n) <- dfs (alter (groups $ map swap rules)) [medicine], m == ["e"]]
