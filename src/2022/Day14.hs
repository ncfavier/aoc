module Day14 where

import AOC

import Data.Map qualified as M

format = eachLine (tile `sepBy` " -> ") where
  tile = (,) <$> decimal <* "," <*> decimal

main :: IO ()
main = do
  paths <- parseInput format
  let grid = M.fromList [(p, '#') | path <- paths, [a, b] <- sort <$> divvy 2 1 path, p <- range (a, b)]
      (_, (_, maxY)) = boundingBox grid
      fall floor grid = go (500, 0) where
        go p@(_, y) | if floor then p `M.member` grid else y > maxY = Nothing
                    | Just p' <- move down <|> move (down + left) <|> move (down + right) = go p'
                    | otherwise = Just $ M.insert p 'o' grid
                    where move d | (p + d) `M.member` grid || snd (p + d) >= maxY + 2 = Nothing
                                 | otherwise = Just (p + d)
  for_ [False, True] \ floor ->
    print $ length $ tail $ iterateMaybe (fall floor) grid
