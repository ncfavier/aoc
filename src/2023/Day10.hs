module Day10 where

import AOC

import Data.Map qualified as M
import Data.Set qualified as S

main = do
  (grid, width, height) <- makeGrid' <$> readInput
  -- Find the tiles in the loop by following the pipe from S.
  let start = head $ M.keys $ M.filter (== 'S') grid
      next i = case grid M.! i of
        'S' -> [n | d <- cardinal, let n = i + d, Just t <- [grid M.!? n], (-d) `elem` connections t]
        t -> (i +) <$> connections t
      steps = bfs next [start]
      loop = S.fromList (fst <$> steps)
  print $ snd $ last steps
  -- Flood the grid from the outside, including loop tiles, while taking care not to "leak" inside.
  -- This requires keeping track of the *orientation* of each loop tile (i.e. which way is inward),
  -- which can be seen as moving along ℝP¹ while keeping track of a point in the double cover by S¹
  -- (some discrete approximation of that, anyway).
  let start = [(i, Outside) | i <- toList (outerEdge width height)]
      move i d = grid M.!? n <&> \ t ->
        (n, if n `S.member` loop then On (-d) else Outside)
        where n = i + d
      next (i, On norm) = [(i + d, On (transport d t norm)) | d <- connections t] -- From a point on the loop, we reach connected pipes
                       ++ [(i + d, Outside) | d <- normToOutside t norm, (i + d) `S.notMember` loop] -- ...and the outside.
        where t = grid M.! i
      next (i, Outside) = catMaybes (move i <$> cardinal)
      outside = S.fromList $ fst . fst <$> bfsOn fst next start
      inside = grid `M.withoutKeys` outside
  print $ length inside

connections :: Char -> [Coords]
connections '|' = [up, down]
connections '-' = [left, right]
connections 'J' = [up, left]
connections 'L' = [up, right]
connections '7' = [down, left]
connections 'F' = [down, right]
connections _ = []

-- Our "normal vectors" are just cardinal directions pointing away from the loop.
type Normal = Coords

data Location = Outside | On Normal

xcw, xccw, ycw, yccw :: Normal -> Normal
xcw  n | n == up = right
       | n == down = left
       | otherwise = n
xccw n | n == up = left
       | n == down = right
       | otherwise = n
ycw  n | n == left = up
       | n == right = down
       | otherwise = n
yccw n | n == left = down
       | n == right = up
       | otherwise = n

-- Transport a normal vector along the given direction in the fibration S¹ → ℝP¹.
-- Since this is a double cover, going from '-' to '-' will flip the orientation.
transport :: Coords -> Char -> Normal -> Normal
transport d '|' n = n
transport d '-' n = n
transport d 'J' n | d == up = xccw n
                  | d == left = ycw n
transport d 'L' n | d == up = xcw n
                  | d == right = yccw n
transport d 'F' n | d == down = xccw n
                  | d == right = ycw n
transport d '7' n | d == down = xcw n
                  | d == left = yccw n

-- Literal corner cases.
normToOutside :: Char -> Normal -> [Coords]
normToOutside 'S' n = [n]
normToOutside '-' n = [n]
normToOutside '|' n = [n]
normToOutside 'J' n = [xccw n, ycw n]
normToOutside 'F' n = [xccw n, ycw n]
normToOutside 'L' n = [xcw n, yccw n]
normToOutside '7' n = [xcw n, yccw n]
