module Day02 where

import AOC

import Data.Modular
import Numeric.Lens

format = eachLine ((,) <$> anySingle <* " " <*> anySingle)

data RPS = Rock | Paper | Scissors deriving (Eq, Enum, Show)

mod3 :: Enum a => Iso' a (Int `Mod` 3)
mod3 = from enum . iso (toMod @3) unMod

moveOutcome :: RPS -> Iso' RPS Ordering
moveOutcome m = mod3 . adding (1 - view mod3 m) . from mod3

opponentMove = \case 'A' -> Rock; 'B' -> Paper; 'C' -> Scissors
myMove       = \case 'X' -> Rock; 'Y' -> Paper; 'Z' -> Scissors
outcome      = \case 'X' -> LT;   'Y' -> EQ;    'Z' -> GT

moveScore    = \case Rock -> 1; Paper -> 2; Scissors -> 3
outcomeScore = \case LT -> 0; EQ -> 3; GT -> 6

score1 (op, me) = moveScore me + outcomeScore (view (moveOutcome op) me)
score2 (op, ou) = moveScore (view (from (moveOutcome op)) ou) + outcomeScore ou

main :: IO ()
main = do
  strategy <- parseInput format
  print $ sum $ map score1 $ map (opponentMove *** myMove) strategy
  print $ sum $ map score2 $ map (opponentMove *** outcome) strategy
