module Day17 where

import AOC

import Data.Map qualified as M

data State = State !Coords !Coords !Int deriving (Eq, Ord, Show)

orthogonal d | d `elem` [up, down] = [left, right]
             | otherwise = [up, down]

main = do
  (fmap digitToInt -> cost, width, height) <- makeGrid' <$> readInput
  let
    start = [State (0, 0) right 0, State (0, 0) down 0]
    end = (width - 1, height - 1)
    next (i, a) (State p d s) =
      [ (n, c, fromInteger (manhattan (p' - end)))
      | n@(State p' _ _) <- neighbours
      , Just c <- [cost M.!? p']]
      where neighbours = [State (p + d') d' 0 | d' <- orthogonal d, s >= i]
                      <> [State (p + d) d (succ s) | succ s < a]
  for [(0, 3), (3, 10)] \(i, a) -> do
    let steps = astar (next (i, a)) start
    print $ head [n | (State p _ s, n) <- steps, p == end, s >= i]
