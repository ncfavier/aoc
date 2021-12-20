module Day20 where

import AOC
import Data.Map qualified as M
import Data.Set qualified as S

format :: Parser (Set Int, Set Coords, Integer, Integer)
format = do
  algorithm <- M.fromList . zip [0..] <$> many printChar <* newline <* newline
  (grid, width, height) <- makeGrid' <$> takeRest
  pure (toSet algorithm, toSet grid, width, height)
  where toSet = mapToSet (== '#')

main :: IO ()
main = do
  (algorithm, grid, width, height) <- parseInput format
  let isLit = memo2 \case
        0 -> (`S.member` grid)
        n -> \(x, y) -> fromBits [isLit (n - 1) (x + j, y + i) | i <- [-1..1], j <- [-1..1]] `S.member` algorithm
  for_ [2, 50] \n ->
    print $ howMany (isLit n) $ (,) <$> [-n..width+n-1] <*> [-n..height+n-1]
