{-# LANGUAGE TypeFamilies #-}
module AOC ( module AOC
           , module Control.Applicative
           , module Control.Arrow
           , module Control.Monad
           , module Control.Monad.Combinators.Expr
           , module Data.Bits
           , module Data.Char
           , module Data.Either
           , module Data.Foldable
           , module Data.Functor
           , module Data.Functor.Identity
           , module Data.Function
           , module Data.Ix
           , module Data.List
           , module Data.List.Split
           , module Data.Map
           , module Data.Maybe
           , module Data.Ord
           , module Data.Ratio
           , module Data.Semigroup
           , module Data.Set
           , module Data.Traversable
           , module Data.Tuple
           , module GHC.Natural
           , module Math.NumberTheory.Moduli
           , module Text.Megaparsec
           , module Text.Megaparsec.Char
           , module Text.Read
           ) where

import Control.Applicative
import Control.Arrow hiding (left, right)
import Control.Lens
import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Bits
import Data.Char
import Data.Either
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Functor.Identity
import Data.Ix
import Data.List
import Data.List.Split (splitOn, chunksOf)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Strict qualified as MapS
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.PriorityQueue.FingerTree qualified as PQ
import Data.Ratio
import Data.Semigroup hiding (option)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable
import Data.Tuple
import Data.Void
import GHC.Natural
import Math.NumberTheory.Moduli
import System.Environment
import System.Exit
import Text.Megaparsec hiding (State(..), Pos, parseMaybe, oneOf, noneOf, choice, many, some)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as Lex
import Text.Read (readMaybe)

-- Parsing

readInput :: IO String
readInput = maybe getContents readFile =<< lookupEnv "AOC_INPUT"

type Parser = Parsec Void String

parseLines :: String -> Parser a -> Parser [a]
parseLines s p = for (lines s) \line ->
    setInput line *> p <* setInput "\n" <* newline <* eof

parseIO :: Parser a -> String -> IO a
parseIO p s = do
    case runParser (p <* eof) "" s of
        Left e -> die (errorBundlePretty e)
        Right a -> pure a

parseIOLines :: Parser a -> String -> IO [a]
parseIOLines p s = parseIO (parseLines s p) s

parseInput :: Parser a -> IO a
parseInput p = parseIO p =<< readInput

parseInputLines :: Parser a -> IO [a]
parseInputLines p = do
    s <- readInput
    parseIOLines p s

lexeme, lexeme' :: Parser a -> Parser a
lexeme p = p <* skipMany spaceChar
lexeme' p = p <* skipMany (char ' ')

word :: Parser String
word = some letterChar

decimal, binary, octal, hexadecimal :: Num a => Parser a
decimal     = Lex.decimal
binary      = Lex.binary
octal       = Lex.octal
hexadecimal = Lex.hexadecimal

number :: Num a => Parser a
number = Lex.signed (pure ()) Lex.decimal

numberInRange :: (Ix a, Num a) => (a, a) -> Parser a
numberInRange r = mfilter (inRange r) number

parseMaybe :: Parser a -> String -> Maybe a
parseMaybe = P.parseMaybe

oneOf :: String -> Parser Char
oneOf = P.oneOf

noneOf :: String -> Parser Char
noneOf = P.noneOf

infixl 3 <||>
(<||>) :: Parser a -> Parser a -> Parser a
a <||> b = try a <|> b

choice :: Foldable t => t (Parser a) -> Parser a
choice = foldr (<||>) empty

-- Lists and maps

notNull :: Foldable t => t a -> Bool
notNull = not . null

howMany :: (Num n, Foldable t) => (a -> Bool) -> t a -> n
howMany p = foldl' (\c e -> if p e then c + 1 else c) 0

counts :: (Num n, Foldable t, Ord a) => t a -> Map a n
counts = foldl' (\m e -> Map.insertWith (+) e 1 m) Map.empty

groups :: Ord k => [(k, a)] -> Map k [a]
groups kv = Map.fromListWith (<>) [(k, pure v) | (k, v) <- kv]

minimumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> a
minimumOn = minimumBy . comparing

maximumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> a
maximumOn = maximumBy . comparing

iterate1 :: (a -> a) -> a -> [a]
iterate1 f x = iterate f (f x)

findDuplicatesBy :: Ord b => (a -> b) -> [a] -> [a]
findDuplicatesBy f = go Set.empty where
    go seen (x:xs) | r `Set.member` seen = x:xs'
                   | otherwise = xs'
                   where xs' = go (Set.insert r seen) xs
                         r = f x
    go _ [] = []

findDuplicates :: Ord a => [a] -> [a]
findDuplicates = findDuplicatesBy id

firstDuplicateBy :: Ord b => (a -> b) -> [a] -> a
firstDuplicateBy f = head . findDuplicatesBy f

firstDuplicate :: Ord a => [a] -> a
firstDuplicate = firstDuplicateBy id

pickOne :: [a] -> [(a, [a])]
pickOne l = [(y, xs ++ ys) | (xs, y:ys) <- zip (inits l) (tails l)]

pickSubset :: Ord a => Integer -> Set a -> [(Set a, Set a)]
pickSubset 0 s = pure (Set.empty, s)
pickSubset n s = do
    (x, s') <- maybeToList (Set.minView s)
    let pick = do
            (s1, s2) <- pickSubset (pred n) s'
            pure (Set.insert x s1, s2)
        don'tPick = do
            (s1, s2) <- pickSubset n s'
            pure (s1, Set.insert x s2)
    pick <|> don'tPick

-- Functions and memoizing

nTimes :: (a -> a) -> Integer -> a -> a
nTimes _ 0 = id
nTimes f 1 = f
nTimes f n = f . nTimes f (pred n)

fixMem :: (Ord k, Foldable t) => t k -> ((k -> a) -> k -> a) -> k -> a
fixMem keys f = go where
    mem = Map.fromSet go (foldMap Set.singleton keys)
    go = f \x -> Map.findWithDefault (go x) x mem

böb :: ASetter s t a b -> s -> (t -> a -> b) -> t
böb l s f = go where
    go = s & l %~ f go

-- Lenses and traversals

traverseSet :: (Applicative f, Ord b) => (a -> f b) -> Set a -> f (Set b)
traverseSet f = fmap Set.fromList . traverse f . Set.toList

-- 2D coordinates and grids

type Coords = (Integer, Integer)

instance Num Coords where
    (x1, y1) + (x2, y2) = (x1 + x2, y1 + y2)
    (x1, y1) * (x2, y2) = (x1 * x2, y1 * y2)
    negate (x, y) = (-x, -y)
    fromInteger n = (n, n)
    abs (x, y) = (abs x, abs y)
    signum = undefined

clamp :: Coords -> Coords -> Coords -> Coords
clamp (xmin, ymin) (xmax, ymax) (x, y) = (min xmax (max xmin x), min ymax (max ymin y))

mul :: Integer -> Coords -> Coords
mul n p = fromInteger n * p

ccw, cw :: Coords -> Coords
ccw (x, y) = (y, -x)
cw  (x, y) = (-y, x)

manhattan :: Coords -> Integer
manhattan (x, y) = abs x + abs y

up, right, down, left :: Coords
right = (1, 0)
down = (0, 1)
left = -right
up = -down

cardinal :: [Coords]
cardinal = [up, right, down, left]

interCardinal :: [Coords]
interCardinal = [(-1, -1), (1, 1), (-1, 1), (1, -1)]

principal :: [Coords]
principal = cardinal ++ interCardinal

angle :: RealFloat n => Coords -> n
angle (x, y) = -atan2 (fromIntegral x) (fromIntegral y)

flattenWithCoords :: [[a]] -> [((Integer, Integer), a)]
flattenWithCoords rows = [ ((x, y), a)
                         | (y, row) <- zip [0..] rows
                         , (x, a)   <- zip [0..] row
                         ]

makeGrid :: Num n => String -> (Map (Integer, Integer) Char, n, n)
makeGrid s = (grid, width, height) where
    rows = lines s
    grid = Map.fromList (flattenWithCoords rows)
    width = genericLength (head rows)
    height = genericLength rows

gridToSet :: (a -> Bool) -> Map Coords a -> Set Coords
gridToSet p = Map.keysSet . Map.filter p

class GridLike a where
    getCoords :: a -> [Coords]
    mapCoords :: (Coords -> Coords) -> a -> a

instance GridLike (Set Coords) where
    getCoords = Set.toList
    mapCoords = Set.map

instance GridLike (Map Coords a) where
    getCoords = Map.keys
    mapCoords = Map.mapKeys

boundingBox :: GridLike a => a -> (Coords, Coords)
boundingBox g = (i, a) where
    i = foldl1 (\(ix, iy) (x, y) -> (ix `min` x, iy `min` y)) c
    a = foldl1 (\(ax, ay) (x, y) -> (ax `max` x, ay `max` y)) c
    c = getCoords g

dimensions :: GridLike a => a -> (Integer, Integer)
dimensions g = (maxX - minX + 1, maxY - minY + 1) where
    ((minX, minY), (maxX, maxY)) = boundingBox g

rotateGrid :: GridLike a => a -> a
rotateGrid g = mapCoords (\(x, y) -> (y, width - x - 1)) g where
    (width, _) = dimensions g

flipGrid :: GridLike a => a -> a
flipGrid = mapCoords swap

-- Cellular automata

data Cell = Cell !Bool !Int

instance Semigroup Cell where
    Cell a1 n1 <> Cell a2 n2 = Cell (a1 || a2) (n1 + n2)

evolve :: Ord a => (a -> [a]) -> (Cell -> Bool) -> Set a -> Set a
evolve neighbours rule alive = MapS.keysSet (MapS.filter rule cells) where
    cells = MapS.fromListWith (<>) (foldMap neighbourhood alive)
    neighbourhood p = [ (p', Cell self (1 - fromEnum self))
                      | p' <- neighbours p
                      , let self = p == p' ]

-- Graph exploration

dfs :: (Num n, Ord a) => (a -> [a]) -> a -> [(a, n)]
dfs = dfsOn id

dfsOn :: (Num n, Ord b) => (a -> b) -> (a -> [a]) -> a -> [(a, n)]
dfsOn rep next start = go Set.empty (Seq.singleton (start, 0)) where
    go _ Seq.Empty = []
    go seen (ps Seq.:|> (n, d))
        | r `Set.member` seen = go seen ps
        | otherwise           = (n, d):go (Set.insert r seen) (ps <> Seq.fromList [(n', d + 1) | n' <- next n])
        where r = rep n

bfs :: (Num n, Ord a) => (a -> [a]) -> a -> [(a, n)]
bfs = bfsOn id

bfsOn :: (Num n, Ord b) => (a -> b) -> (a -> [a]) -> a -> [(a, n)]
bfsOn rep next start = go Set.empty (Seq.singleton (start, 0)) where
    go _ Seq.Empty = []
    go seen ((n, d) Seq.:<| ps)
        | r `Set.member` seen = go seen ps
        | otherwise           = (n, d):go (Set.insert r seen) (ps <> Seq.fromList [(n', d + 1) | n' <- next n])
        where r = rep n

dijkstra :: (Num n, Ord n, Ord a) => (a -> [(a, n)]) -> a -> [(a, n)]
dijkstra = dijkstraOn id

dijkstraOn :: (Num n, Ord n, Ord b) => (a -> b) -> (a -> [(a, n)]) -> a -> [(a, n)]
dijkstraOn rep next start = go Set.empty (PQ.singleton 0 start) where
    go seen q
        | Just ((d, n), q') <- PQ.minViewWithKey q =
            let r = rep n in
            if r `Set.member` seen then
                go seen q'
            else
                (n, d):go (Set.insert r seen) (PQ.union q' (PQ.fromList [(d + c, n') | (n', c) <- next n]))
        | otherwise = []
