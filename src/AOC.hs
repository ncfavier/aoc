{-# LANGUAGE TypeFamilies #-}
module AOC ( module AOC
           , module Control.Applicative
           , module Control.Monad
           , module Control.Arrow
           , module Data.Ord
           , module Data.Ix
           , module Data.Char
           , module Data.List
           , module Data.List.Split
           , module Data.Foldable
           , module Data.Function
           , module Data.Traversable
           , module Text.Megaparsec
           , module Text.Megaparsec.Char
           , module Text.Megaparsec.Char.Lexer
           ) where

import           Control.Applicative
import           Control.Arrow hiding (left, right)
import           Control.Monad
import           Data.Char
import           Data.Foldable
import           Data.Function
import           Data.Ix
import           Data.List
import           Data.List.Split (splitOn, chunksOf)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Ord
import           Data.PriorityQueue.FingerTree as PQ
import           Data.Sequence (Seq(..))
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import           Data.Traversable
import           Data.Void
import           System.Environment
import           System.Exit
import           Text.Megaparsec hiding (count, count', many, some)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer hiding (space)

-- Parsing

readInput :: IO String
readInput = maybe getContents readFile =<< lookupEnv "AOC_INPUT"

type Parser = Parsec Void String

parseIO :: Parser a -> String -> IO a
parseIO p s = do
    case runParser (p <* eof) "" s of
        Left e -> die (errorBundlePretty e)
        Right a -> pure a

parseInput :: Parser a -> IO a
parseInput p = parseIO p =<< readInput

parseInputLines :: Parser a -> IO [a]
parseInputLines p = do
    s <- readInput
    let p' = for (lines s) \line ->
            setInput (line ++ "\n") *> p <* newline <* eof
    parseIO p' s

number :: Num a => Parser a
number = signed (pure ()) decimal

-- Coordinates

type Coords = (Integer, Integer)

left, right, up, down :: Coords -> Coords
left  = (pred *** id)
right = (succ *** id)
up    = (id *** pred)
down  = (id *** succ)

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

makeGrid :: Num n => String -> (Map (Integer, Integer) Char, n, n)
makeGrid s = (grid, width, height) where
    rows = lines s
    grid = Map.fromList (flatten rows)
    width = genericLength (head rows)
    height = genericLength rows

-- List utilities

count :: (Num n, Foldable t) => (a -> Bool) -> t a -> n
count p = foldl' (\c e -> if p e then c + 1 else c) 0

counts :: (Num n, Foldable t, Ord a) => t a -> Map a n
counts = foldl' (\m e -> Map.insertWith (+) e 1 m) Map.empty

findDuplicates :: Ord a => [a] -> [a]
findDuplicates = go Set.empty where
    go seen (x:xs) | x `Set.member` seen = x:xs'
                   | otherwise = xs'
                   where xs' = go (Set.insert x seen) xs
    go _ [] = []

firstDuplicate :: Ord a => [a] -> a
firstDuplicate = head . findDuplicates

pickOne :: [a] -> [(a, [a])]
pickOne l = [(y, xs ++ ys) | (xs, y:ys) <- zip (inits l) (tails l)]

-- Graph exploration

bfs :: (Num n, Ord a) => (a -> [a]) -> a -> [(a, n)]
bfs = bfsOn id

bfsOn :: (Num n, Ord b) => (a -> b) -> (a -> [a]) -> a -> [(a, n)]
bfsOn rep next start = go Set.empty (Seq.singleton (start, 0)) where
    go seen Empty = []
    go seen ((n, d) :<| ps)
        | r `Set.member` seen = go seen ps
        | otherwise         = (n, d):go (Set.insert r seen) (ps <> Seq.fromList [(n', d + 1) | n' <- next n])
        where r = rep n

dijkstra :: (Num n, Ord n, Ord a) => (a -> [(a, n)]) -> a -> [(a, n)]
dijkstra = dijkstraOn id

dijkstraOn :: (Num n, Ord n, Ord b) => (a -> b) -> (a -> [(a, n)]) -> a -> [(a, n)]
dijkstraOn rep next start = go Set.empty (PQ.singleton 0 start) where
    go seen q
        | Just ((d, n), q') <- minViewWithKey q =
            let r = rep n in
            if r `Set.member` seen then
                go seen q'
            else
                (n, d):go (Set.insert r seen) (PQ.union q' (PQ.fromList [(d + c, n') | (n', c) <- next n]))
        | otherwise = []
