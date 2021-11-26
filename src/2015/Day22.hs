module Day22 where

import Data.Map qualified as Map
import Data.Set qualified as Set

import AOC

data Spell = MagicMissile | Drain | Shield | Poison | Recharge
  deriving (Eq, Ord, Enum, Bounded, Show)

data Difficulty = Easy | Hard
  deriving (Eq, Ord, Enum, Bounded, Show)

data Game = Game { difficulty :: Difficulty
                 , _playerHp, _playerMana, _bossHp, bossDamage :: Int
                 , _timers :: Spell `Map` Int
                 } deriving (Eq, Ord, Show)

makeLenses ''Game

manaCost MagicMissile = 53
manaCost Drain        = 73
manaCost Shield       = 113
manaCost Poison       = 173
manaCost Recharge     = 229

loseHp n = guard . (> 0) =<< playerHp <-= n
loseMana n = do
  cost' (Sum n)
  guard . (>= 0) =<< playerMana <-= n

hardMode = do
  Game{difficulty} <- get
  when (difficulty == Hard) do loseHp 1

effects = do
  effs <- timers <<%= Map.filter (> 0) . Map.map pred
  for_ (Map.keys effs) \case
    Recharge -> playerMana += 101
    Poison -> bossHp -= 3
    _ -> pure ()

playerTurn = do
  spell <- alt enumerate
  loseMana (manaCost spell)
  let effect n = guard . isNothing =<< timers.at spell <<?= n
  case spell of
    MagicMissile -> bossHp -= 4
    Drain -> bossHp -= 2 >> playerHp += 2
    Shield -> effect 6
    Poison -> effect 6
    Recharge -> effect 5

bossTurn = do
  Game{bossDamage} <- get
  armor <- bool 0 7 . has (timers.ix Shield) <$> get
  loseHp (max 1 (bossDamage - armor))

game = loop [hardMode >> effects, playerTurn >> effects, bossTurn]
  where
    loop = foldr run empty . cycle . zip [0..]
    run (n, step) rest = do
      g <- get
      lift $ guard . isNothing =<< at (g, n) <<?= ()
      pure g <|> (step >> rest)

wins = mfilter ((<= 0) . view bossHp) game

minimumCostToWin start = c
  where Just (Sum c, _) = evalState (runSearchBestT (evalStateT wins start)) Set.empty

main = do
  (_bossHp, bossDamage) <- parseInput do
    (,) <$ "Hit Points: " <*> number <* newline <* "Damage: " <*> number <* newline
  for_ enumerate \difficulty -> do
    print $ minimumCostToWin Game
      { difficulty
      , _playerHp = 50, _playerMana = 500
      , _bossHp, bossDamage
      , _timers = Map.empty
      }
