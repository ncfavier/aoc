module Day02 where

import AOC

format = eachLine ((,) <$> anySingle <* " " <*> anySingle)

data RPS = Rock | Paper | Scissors deriving (Eq, Show)

instance Ord RPS where
  compare :: RPS -> RPS -> Ordering
  compare x y | x == y = EQ
  compare Scissors Paper = GT
  compare Paper Rock = GT
  compare Rock Scissors = GT
  compare _ _ = LT

-- | @uncompare x@ is an inverse to @(`compare` x)@.
uncompare :: RPS -> Ordering -> RPS
uncompare x EQ = x
uncompare Rock LT = Scissors
uncompare Rock GT = Paper
uncompare Paper LT = Rock
uncompare Paper GT = Scissors
uncompare Scissors LT = Paper
uncompare Scissors GT = Rock

opponentMove = \case 'A' -> Rock; 'B' -> Paper; 'C' -> Scissors
myMove       = \case 'X' -> Rock; 'Y' -> Paper; 'Z' -> Scissors
outcome      = \case 'X' -> LT;   'Y' -> EQ;    'Z' -> GT

moveScore    = \case Rock -> 1; Paper -> 2; Scissors -> 3
outcomeScore = \case LT -> 0; EQ -> 3; GT -> 6

score1 (op, me) = moveScore me + outcomeScore (compare me op)
score2 (op, ou) = moveScore (uncompare op ou) + outcomeScore ou

main :: IO ()
main = do
  strategy <- parseInput format
  print $ sum $ map score1 $ map (opponentMove *** myMove) strategy
  print $ sum $ map score2 $ map (opponentMove *** outcome) strategy
