module Day18 where

import AOC

import Data.Array.IArray
import Data.Array.Unboxed
import Data.Set qualified as S

format = eachLine ((,,) <$> number <* "," <*> number <* "," <*> number)

main :: IO ()
main = do
  tiles <- S.fromList <$> parseInput format
  let lo = foldl1 (\(xmin, ymin, zmin) (x, y, z) -> (xmin `min` x, ymin `min` y, zmin `min` z)) tiles
      hi = foldl1 (\(xmax, ymax, zmax) (x, y, z) -> (xmax `max` x, ymax `max` y, zmax `max` z)) tiles
      grid = array @UArray (lo, hi) [(c, c `S.member` tiles) | c <- range (lo, hi)]
      neighbours c = (c +) <$> cardinal3
      outside = S.fromList $ fst <$> bfs (filter (\c -> inRange (lo, hi) c && not (grid ! c)) . neighbours) [lo]
      sides c = 6 - howMany (grid !) (filter (inRange (lo, hi)) $ neighbours c)
      freeSides c = 6 - howMany (`S.notMember` outside) (neighbours c)
  print $ sum $ sides <$> S.toList tiles
  print $ sum $ freeSides <$> S.toList tiles
