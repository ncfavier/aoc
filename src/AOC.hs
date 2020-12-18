{-# LANGUAGE TypeFamilies #-}
module AOC ( module AOC
           , module Control.Applicative
           , module Control.Arrow
           , module Control.Monad
           , module Control.Monad.Combinators.Expr
           , module Data.Char
           , module Data.Foldable
           , module Data.Function
           , module Data.Ix
           , module Data.List
           , module Data.List.Split
           , module Data.Map
           , module Data.Maybe
           , module Data.Ord
           , module Data.Semigroup
           , module Data.Set
           , module Data.Traversable
           , module Text.Megaparsec
           , module Text.Megaparsec.Char
           , module Text.Read
           ) where

import Control.Applicative
import Control.Arrow hiding (left, right)
import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Char
import Data.Foldable
import Data.Function
import Data.Ix
import Data.List
import Data.List.Split (splitOn, chunksOf)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Ord
import Data.PriorityQueue.FingerTree qualified as PQ
import Data.Semigroup hiding (option)
import Data.Sequence (Seq(..))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable
import Data.Void
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

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme (Lex.space space1 empty empty)

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

-- List utilities

notNull :: Foldable t => t a -> Bool
notNull = not . null

howMany :: (Num n, Foldable t) => (a -> Bool) -> t a -> n
howMany p = foldl' (\c e -> if p e then c + 1 else c) 0

counts :: (Num n, Foldable t, Ord a) => t a -> Map a n
counts = foldl' (\m e -> Map.insertWith (+) e 1 m) Map.empty

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

-- Function utilities

nTimes :: (a -> a) -> Integer -> a -> a
nTimes f 0 = id
nTimes f 1 = f
nTimes f n = f . nTimes f (pred n)

fixMem :: (Ord k, Foldable t) => t k -> ((k -> a) -> k -> a) -> k -> a
fixMem keys f = go where
    mem = Map.fromSet go (foldMap Set.singleton keys)
    go = f \x -> Map.findWithDefault (go x) x mem

-- Lens utilities

traverseSet f = fmap Set.fromList . traverse f . Set.toList

-- 2D coordinates

type Coords = (Integer, Integer)

instance Num Coords where
    (x1, y1) + (x2, y2) = (x1 + x2, y1 + y2)
    (*) = undefined
    fromInteger = undefined
    negate = undefined
    abs = undefined
    signum = undefined

mul :: Integer -> Coords -> Coords
mul n (x, y) = (n * x, n * y)

ccw, cw :: Coords -> Coords
ccw (x, y) = (y, -x)
cw  (x, y) = (-y, x)

manhattan :: Coords -> Integer
manhattan (x, y) = abs x + abs y

cardinal :: [Coords]
cardinal = [(-1, 0), (1, 0), (0, -1), (0, 1)]

interCardinal :: [Coords]
interCardinal = [(-1, -1), (1, 1), (-1, 1), (1, -1)]

principal :: [Coords]
principal = cardinal ++ interCardinal

left, right, up, down :: Coords -> Coords
[left, right, up, down] = map (+) cardinal

angle :: RealFloat n => Coords -> n
angle (x, y) = -atan2 (fromIntegral x) (fromIntegral y)

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

-- Graph exploration

bfs :: (Num n, Ord a) => (a -> [a]) -> a -> [(a, n)]
bfs = bfsOn id

bfsOn :: (Num n, Ord b) => (a -> b) -> (a -> [a]) -> a -> [(a, n)]
bfsOn rep next start = go Set.empty (Seq.singleton (start, 0)) where
    go seen Empty = []
    go seen ((n, d) :<| ps)
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
