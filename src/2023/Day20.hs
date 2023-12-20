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

type S = Map String Module

main = do
  (mods, conns) <- parseInput format
  let
    pushButton :: StateT S (Writer [(String, Bool, String)]) ()
    pushButton = go (Seq.singleton ("button", low, "broadcaster")) where
      update from p to Broadcast = pure [p]
      update from p to (FlipFlop s)
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
  let pulses = execWriter $ runStateT (replicateM 1000 pushButton) mods
  print $ product $ counts [b | (_, b, _) <- pulses]

  -- let pulses = execWriter $ runStateT (replicateM 100000 pushButton) mods
  --     go n [] = []
  --     go n (("button", _, _):ps) = go (n + 1) ps
  --     go n (("zb", True, _):ps) = n:go n ps
  --     go n (_:ps) = go n ps
  -- print $ go 0 pulses

  -- putStrLn "digraph {"
  -- for_ (M.assocs mods) \ (m, t) -> do
  --   putStrLn $ m <> " [shape=" <> (case t of Broadcast -> "circle"; Conjunction _ -> "diamond"; _ -> "box") <> "]"
  -- for_ (M.assocs conns) \(m, cs) -> do
  --   for_ cs \c -> do
  --     putStrLn $ m <> " -> " <> c
  -- putStrLn "}"
