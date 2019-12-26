module Day25 where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

import AOC

type Position = (Integer, Integer, Integer, Integer)

manhattan (a1, a2, a3, a4) (b1, b2, b3, b4) = abs (a1 - b1) + abs (a2 - b2) + abs (a3 - b3) + abs (a4 - b4)

unite a b m = M.map convert m
    where
        ra = m M.! a
        rb = m M.! b
        convert p | p == rb   = ra
                  | otherwise = p

update []     = id
update (x:xs) = update xs . foldr (.) id [unite x x' | x' <- xs, manhattan x x' <= 3]

main = do
    points <- map (\t -> read ("(" ++ t ++ ")") :: Position) . lines <$> readInput
    let constellations = M.fromList [(p, p) | p <- points]
    print $ length $ S.fromList $ M.elems $ update points constellations
