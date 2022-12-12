module Day12 where

import AOC

import Data.Map qualified as M

main :: IO ()
main = do
  input <- parseInput $ eachLine $ (,) <$> word <* "-" <*> word
  let adj = groups $ input ++ map swap input
  for_ [False, True] \part2 -> do
    let next ("end":_) = []
        next path@(n:_) = [n':path | n' <- adj M.! n, canVisit n']
          where
            canVisit "start" = False
            canVisit n
              | all isUpper n = True
              | otherwise = case filter (== n) path of
                _:_:_ -> False
                _:_ -> part2 && 2 `notElem` counts (filter (all isLower) path)
                _ -> True
    print $ length [() | ("end":_, _) <- dfs next [["start"]]]
