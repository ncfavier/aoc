module Day17 where

import AOC

format = (,) <$ "target area: x=" <*> range <* ", y=" <*> range <* newline
  where range = (,) <$> number <* ".." <*> number

traj :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
traj (xv, yv) (x, y) = (x, y):traj (signum xv * (abs xv - 1), yv - 1) (x + xv, y + yv)

main :: IO ()
main = do
  ((xmin, xmax), (ymin, ymax)) <- parseInput format
  let valid = any (\(x, y) -> inRange (xmin, xmax) x && inRange (ymin, ymax) y) . takeWhile (\(x, y) -> y >= ymin)
      heights = [sumUpTo vy | vx <- [0..xmax], vy <- [ymin.. -ymin], valid $ traj (vx, vy) (0, 0)]
  print $ maximum heights
  print $ length heights