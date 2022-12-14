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
      start = (500, 0)
      fall floor grid
        | floor && start `M.member` grid = Nothing
        | otherwise = go start
        where go p@(_, y)
                | not floor && y > maxY = Nothing
                | Just p' <- move down <|> move (down + left) <|> move (down + right) = go p'
                | otherwise = Just (M.insert p 'o' grid)
                where move d
                        | y' >= maxY + 2 || p' `M.member` grid = Nothing
                        | otherwise = Just p'
                        where p'@(_, y') = p + d
  for_ [False, True] \ floor ->
    print $ length $ tail $ iterateMaybe (fall floor) grid
