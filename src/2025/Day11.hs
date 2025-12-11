module Day11 where

import AOC

import Data.Map qualified as M
import Data.Set qualified as S

format = eachLine do
  (,) <$> word <* hlexeme ":" <*> many (hlexeme word)

countPaths outputs middle end =
  (M.! middle) . fixMem (M.keysSet outputs) \ go start ->
    if start == end then M.singleton S.empty 1 else
      let c = M.unionsWith (+) (go <$> (outputs M.! start)) in
      if start `elem` middle then M.mapKeys (S.insert start) c else c

main = do
  outputs <- M.fromList <$> parseInput format
  print $ countPaths outputs S.empty "out" "you"
  print $ countPaths outputs (S.fromList ["dac", "fft"]) "out" "svr"
