module Day17 where

import AOC hiding (up, down)
import AOC qualified

import Data.Set qualified as S

format = many (oneOf "<>") <* newline

up = AOC.down
down = AOC.up

rocks :: [Set Coords]
rocks =
  [ {- — -} S.fromList [i `stimes` right | i <- [0..3]]
  , {- + -} S.fromList (map ((+1) *** (+1)) (origin:cardinal))
  , {- ⅃ -} S.fromList [origin, right, right + right, right + right + up, right + right + up + up]
  , {- | -} S.fromList [i `stimes` up | i <- [0..3]]
  , {- ∎ -} S.fromList [origin, right, up, right + up]
  ]

fall (p:pieces) jet grid = fall1 start jet where
  start = (2, highest + 4)
  highest | S.null grid = -1
          | otherwise = snd . snd . boundingBox $ grid
  fall1 c (j:jet)
    | miny >= 0 && S.null (S.intersection p''' grid) = fall1 c''' jet
    | otherwise = fall pieces jet (grid <> p'')
    where
      c' = c + (if j == '<' then left else right)
      p' = mapCoords (+ c') p
      (c'', p'')
        | minx >= 0 && maxx < 7 && S.null (S.intersection p' grid) = (c', p')
        | otherwise = (c, mapCoords (+ c) p)
        where ((minx, _), (maxx, _)) = boundingBox p'
      c''' = c'' + down
      p''' = mapCoords (+ c''') p
      ((_, miny), _) = boundingBox p'''
fall [] _ grid = grid

main :: IO ()
main = do
  jet <- parseInput format
  print $ snd $ dimensions $ fall (take 2022 $ cycle rocks) (cycle jet) S.empty
