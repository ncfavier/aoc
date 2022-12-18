module Day16 where

import AOC

import Data.Map qualified as M
import Data.Set qualified as S

data Agent = Idle String | Moving String Int | Done Int
  deriving (Eq, Ord, Show)

format = M.fromList <$> eachLine valve where
  maybeS n = n <* optional "s"
  valve = (,) <$ "Valve " <*> many letterChar <* " has flow rate=" <*> ((,) <$> number <* maybeS "; tunnel" <* maybeS " lead" <* maybeS " to valve" <* " " <*> many letterChar `sepBy` ", ")

maximumPressure valves time workers = {-[h | (s@(h@(t, _, _)), _) <- steps] -} head [time * sum pressures - p | ((t, _, _), p) <- steps, t == time]
  where
  distances = flip M.mapWithKey valves \ v _ -> M.fromList $ bfs (\v -> snd (valves M.! v)) [v]
  pressures = M.filter (> 0) . fmap fst $ valves
  next (t, agents, open) = do
    let goals = pressures `M.withoutKeys` open
        cost = sum goals
        act (Idle v) = do
          asum ([act (Moving g (distances M.! v M.! g)) | g <- M.keys goals] <> [pure (Done 0) | M.null goals])
        act (Moving g 0) = Idle g <$ modify (S.insert g)
        act (Moving g c) = pure (Moving g (pred c))
        act (Done c) = pure (Done (succ c))
    (agents', open') <- runStateT (traverse act agents) open
    pure ((succ t, agents', open'), cost)
  rep (_, agents, open) = (sort agents, open)
  steps = dijkstraOn rep next [(0, replicate workers (Idle "AA"), S.empty)]

main :: IO ()
main = do
  valves <- parseInput format
  print $ maximumPressure valves 30 1
  print $ maximumPressure valves 26 2
