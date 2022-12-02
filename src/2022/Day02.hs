module Day02 where

import AOC

import Data.Modular
import Numeric.Lens

format = eachLine ((,) <$> anySingle <* " " <*> anySingle)

data RPS = Rock | Paper | Scissors deriving (Eq, Enum, Show)

-- | Assuming @a@ is a type with three elements, then it is isomorphic to the integers mod 3.
mod3 :: Enum a => Iso' a (Int `Mod` 3)
mod3 = from enum . iso toMod unMod

-- | The game of rock-paper-scissors: given one of the players' move, there is a one-to-one
-- correspondence between the other player's moves and the outcomes.
-- The correspondence is given by subtracting the moves mod 3 and adjusting so that equality
-- corresponds to 0.
rps :: RPS -> Iso' RPS Ordering
rps m = mod3 . subtracting (view mod3 m) . adding (view mod3 EQ) . from mod3

opponentMove = \case 'A' -> Rock; 'B' -> Paper; 'C' -> Scissors
myMove       = \case 'X' -> Rock; 'Y' -> Paper; 'Z' -> Scissors
outcome      = \case 'X' -> LT;   'Y' -> EQ;    'Z' -> GT

moveScore    = \case Rock -> 1; Paper -> 2; Scissors -> 3
outcomeScore = \case LT -> 0; EQ -> 3; GT -> 6

score1 (op, me) = moveScore me + outcomeScore (view (rps op) me)
score2 (op, ou) = moveScore (view (from (rps op)) ou) + outcomeScore ou

main :: IO ()
main = do
  strategy <- parseInput format
  print $ sum $ map score1 $ map (opponentMove *** myMove) strategy
  print $ sum $ map score2 $ map (opponentMove *** outcome) strategy
