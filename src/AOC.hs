{-# LANGUAGE TypeFamilies #-}
module AOC ( module AOC
           , module Control.Applicative
           , module Control.Monad
           , module Control.Arrow
           , module Data.Ord
           , module Data.Char
           , module Data.List
           , module Data.List.Split
           , module Data.Void
           , module Data.Function
           , module Text.Megaparsec
           , module Text.Megaparsec.Char
           , module Text.Megaparsec.Char.Lexer
           ) where

import Control.Applicative hiding (some, many)
import Control.Monad
import Control.Arrow hiding (left, right)
import Data.Ord
import Data.Char
import Data.List
import Data.List.Split (splitOn, chunksOf)
import Data.Void
import Data.Function
import Text.Megaparsec hiding (count)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)

import Data.Set (Set)
import qualified Data.Set as S
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.PriorityQueue.FingerTree as PQ

type Parser = Parsec Void String

parseLines p = many (p <* eol)

number :: Integral a => Parser a
number = signed (return ()) decimal

type Coords = (Integer, Integer)

add :: Coords -> Coords -> Coords
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

ccw, cw :: Coords -> Coords
ccw (x, y) = (y, -x)
cw  (x, y) = (-y, x)

flatten :: [[a]] -> [((Integer, Integer), a)]
flatten rows = [ ((x, y), a)
               | (y, row) <- zip [0..] rows
               , (x, a)   <- zip [0..] row
               ]

pickOne :: [a] -> [(a, [a])]
pickOne l = [(y, xs ++ ys) | (xs, y:ys) <- zip (inits l) (tails l)]

bfs :: Ord a => (a -> [a]) -> a -> [(a, Integer)]
bfs = bfsOn id

bfsOn :: Ord b => (a -> b) -> (a -> [a]) -> a -> [(a, Integer)]
bfsOn rep next start = go S.empty (Seq.singleton (start, 0)) where
    go seen Empty = []
    go seen ((n, d) :<| ps)
        | r `S.member` seen = go seen ps
        | otherwise         = (n, d):go (S.insert r seen) (ps <> Seq.fromList [(n', d + 1) | n' <- next n])
        where r = rep n

dijkstra :: Ord a => (a -> [(a, Integer)]) -> a -> [(a, Integer)]
dijkstra = dijkstraOn id

dijkstraOn :: Ord b => (a -> b) -> (a -> [(a, Integer)]) -> a -> [(a, Integer)]
dijkstraOn rep next start = go S.empty (PQ.singleton 0 start) where
    go seen q
        | Just ((d, n), q') <- minViewWithKey q =
            let r = rep n in
            if r `S.member` seen then
                go seen q'
            else
                (n, d):go (S.insert r seen) (PQ.union q' (PQ.fromList [(d + c, n') | (n', c) <- next n]))
        | otherwise = []
