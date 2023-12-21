module Day20 where

import AOC

import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Sequence qualified as Seq

data Module = FlipFlop Bool | Conjunction (Map String All) | Broadcast deriving (Eq, Ord, Show)

low = False
high = True

format = NE.unzip <$> mfix \ a -> M.fromList <$> eachLine mdo
  let
    mod = word
    typ = FlipFlop low <$ char '%' <|> Conjunction (All low <$ M.filter ((source `elem`) . snd) a) <$ char '&' <|> pure Broadcast
  t <- typ
  source <- mod
  " -> "
  dest <- mod `sepBy` ", "
  pure (source, (t, dest))

main = do
  (mods, conns) <- parseInput format
  let
    pushButton :: StateT (Map String Module) (Writer [(String, Bool, String)]) ()
    pushButton = go (Seq.singleton ("button", low, "broadcaster")) where
      update _ p _ Broadcast = pure [p]
      update _ p to (FlipFlop s)
        | p == low = do
          ix to .= FlipFlop (not s)
          pure [not s]
        | otherwise = pure []
      update from p to (Conjunction s) = do
        let s' = s & ix from .~ All p
        ix to .= Conjunction s'
        pure [not $ getAll (fold s')]
      go Seq.Empty = pure ()
      go ((from, p, to) Seq.:<| q) = do
        tell [(from, p, to)]
        use (at to) >>= \case
          Just s -> do
            ps <- update from p to s
            go (q <> Seq.fromList [(to, p, next) | p <- ps, next <- conns M.! to])
          Nothing -> go q
  let
    pulses n = execWriter $ runStateT (replicateM n pushButton) mods
    pulsesInf = execWriter $ runStateT (forever pushButton) mods
  print $ product $ counts [b | (_, b, _) <- pulses 1000]
  let -- the dumb assumptions begin...
    ultimate = head [m | (m, ["rx"]) <- M.assocs conns]
    penultimate = let Conjunction is = mods M.! ultimate in M.keys is
    period m = head (go 0 pulsesInf) where
      go _ [] = []
      go n (("button", _, _):ps) = go (n + 1) ps
      go n ((m', True, _):ps) | m' == m = n:go n ps
      go n (_:ps) = go n ps
  print $ foldl1 lcm $ map period penultimate
