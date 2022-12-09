module Day09 where

import AOC

import Data.Set qualified as S

format = eachLine ((,) <$> letterChar <* " " <*> number)

move :: Char -> Coords
move 'D' = down
move 'U' = up
move 'L' = left
move 'R' = right

type Rope = [Coords]

chase :: Coords -> Coords -> Coords
chase mouse cat
  | inRange ((-1, -1), (1, 1)) (x, y) = origin
  | x == 0, y < 0 = up
  | x == 0, y > 0 = down
  | x < 0, y == 0 = left
  | x > 0, y == 0 = right
  | x < 0, y < 0 = left + up
  | x > 0, y < 0 = right + up
  | x < 0, y > 0 = left + down
  | x > 0, y > 0 = right + down
  where (x, y) = mouse - cat

moveRope :: Coords -> Rope -> Rope
moveRope d (h:t) = h':moveRope (chase h' (head t)) t where h' = h + d
moveRope _ [] = []

main :: IO ()
main = do
  ins <- concatMap (uncurry (flip replicate)) <$> parseInput format
  let go rope (i:is) = last rope : go (moveRope (move i) rope) is
      go rope [] = [last rope]
  print $ length $ S.fromList $ go (replicate 2 origin) ins
  print $ length $ S.fromList $ go (replicate 10 origin) ins
