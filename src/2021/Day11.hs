module Day11 where

import AOC

import Data.Map qualified as M

step width height octopuses = update flashes where
  update flashes = reset $ M.unionWith (+) (fmap (+1) octopuses) $ M.fromListWith (+)
    [ (p', 1)
    | p <- flashes, d <- principal, let p' = p + d
    , inRange ((0, 0), (height - 1, width - 1)) p'
    ]
  flashes = firstDuplicate $ iterate (M.keys . M.filter (== 0) . update) []
  reset = fmap (\n -> if n > 9 then 0 else n)

main :: IO ()
main = do
  (fmap digitToInt -> octopuses, width, height) <- makeGrid <$> readInput
  let history = iterate1 (step width height) octopuses
  print $ howManyOf (each . each) (== 0) (take 100 history)
  print $ head [n | (n, g) <- zip [1..] history, all (== 0) g]
