module Day18 where

import AOC

import Data.Map qualified as M
import Data.Set qualified as S

-- WIP

format = eachLine do (,,) <$> lexeme dir <*> lexeme number <*> between "(#" ")" (many alphaNumChar) where
  dir = up <$ char 'U' <|> left <$ char 'L' <|> down <$ char 'D' <|> right <$ char 'R'

segment p d n = take n (iterate (+ d) p)

main = do
  input <- parseInput format
  let
    trench = go (0, 0) where
      go p [] = S.singleton p
      go p ((d, n, _):xs) = S.union (S.fromList (segment p d n)) (go (p + d * fromIntegral n) xs)
  -- print $ length $ trench input
  let t = trench input
  print $ boundingBox t
