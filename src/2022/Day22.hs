module Day22 where

import AOC

import Data.Map qualified as M

data Instruction = F Int | L | R deriving Show

format = do
  grid <- M.filter (/= ' ') . makeGrid . unlines <$> some (anySingleBut '\n') `sepEndBy` newline
  newline
  path <- many (F <$> decimal <|> L <$ "L" <|> R <$ "R") <* newline
  pure (grid, path)

facingToInt f | f == right = 0
              | f == down = 1
              | f == left = 2
              | f == up = 3

main :: IO ()
main = do
  (grid, path) <- parseInput format
  let start = head [(x, 0) | (x, 0) <- M.keys grid]
      move (p, f) (F n) = (nTimes step n p, f) where
        step p = if grid M.! p' == '#' then p else p' where
          p' = wrap (p + f)
          wrap p | p `M.member` grid = p
                 | otherwise = head [p' + f | p' <- iterate1 (subtract f) p, p' `M.notMember` grid]
      move (p, f) L = (p, ccw f)
      move (p, f) R = (p, cw f)
      ((x, y), f) = foldl' move (start, right) path
  print (1000 * succ y + 4 * succ x + facingToInt f)
