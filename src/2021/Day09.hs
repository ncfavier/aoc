module Day09 where

import AOC

import Data.Map qualified as M
import Data.Set qualified as S

main :: IO ()
main = do
  (fmap digitToInt -> grid) <- makeGrid <$> readInput
  let riskLevel pos n = if all (> n) (catMaybes [M.lookup p grid | d <- cardinal, let p = pos + d]) then n + 1 else 0
      basin p = fst <$> bfs (\p' -> [p'' | d <- cardinal, let p'' = p' + d, Just n <- [M.lookup p'' grid], n < 9]) p
      basins = go (M.keysSet $ M.filter (/= 9) grid) where
        go ps | Just (basin -> b) <- S.lookupMin ps = length b : go (ps S.\\ S.fromList b)
              | otherwise = []
  print $ sum $ M.mapWithKey riskLevel grid
  print $ product $ take 3 $ sortBy (flip compare) basins
