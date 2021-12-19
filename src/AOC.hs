module AOC ( module AOC
           , module Control.Applicative
           , module Control.Arrow
           , module Control.Lens
           , module Control.Monad
           , module Control.Monad.Combinators.Expr
           , module Control.Monad.Search
           , module Control.Monad.State
           , module Data.Bits
           , module Data.Bool
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
           , module Data.Void
           , module GHC.Natural
           , module Linear
           , module Math.NumberTheory.Moduli
           , module Text.Megaparsec
           , module Text.Megaparsec.Char
           , module Text.Megaparsec.Char.Lexer
           , module Text.Read
           ) where

import Control.Applicative
import Control.Arrow hiding (left, right)
import Control.Lens hiding (noneOf, index)
import Control.Monad
import Control.Monad.Combinators.Expr
import Control.Monad.Search
import Control.Monad.State
import Data.Bits
import Data.Bool
import Data.Char
import Data.Either
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Functor.Identity
import Data.Ix
import Data.List hiding (uncons)
import Data.List.Split (splitOn, chunksOf)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Strict qualified as MapS
import Data.Maybe
import Data.Monoid
import Data.Ord hiding (Down(..))
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
import Linear hiding (transpose, rotate)
import Math.NumberTheory.Moduli
import System.Environment
import System.Exit
import Text.Megaparsec hiding (State(..), Pos, choice, many, some, takeP, takeWhileP, takeWhile1P)
import Text.Megaparsec qualified
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (signed, binary, octal, decimal, hexadecimal)
import Text.Read (readMaybe)

instance (Ord c, Monoid c, Monad m) => MonadFail (SearchT c m) where fail _ = empty

-- Parsing

readInput :: IO String
readInput = maybe getContents readFile =<< lookupEnv "AOC_INPUT"

type Parser = Parsec Void String

parseIO :: Parser a -> String -> IO a
parseIO p s = case runParser (p <* eof) "" s of
  Left e -> die (errorBundlePretty e)
  Right a -> pure a

parseInput :: Parser a -> IO a
parseInput p = parseIO p =<< readInput

-- | Runs a parser on each input line isolated from the others.
eachLine :: Parser a -> Parser [a]
eachLine p = do
  input <- getInput
  for (lines input) \line ->
    setInput line *> p <* setInput "\n" <* newline <* eof

takeP       = Text.Megaparsec.takeP Nothing
takeWhileP  = Text.Megaparsec.takeWhileP Nothing
takeWhile1P = Text.Megaparsec.takeWhile1P Nothing

lexeme, hlexeme :: Parser a -> Parser a
lexeme p = p <* space
hlexeme p = p <* hspace

word :: Parser String
word = some letterChar

number :: Num a => Parser a
number = signed (pure ()) decimal

numberInRange :: (Ix a, Num a) => (a, a) -> Parser a
numberInRange r = mfilter (inRange r) number

infixl 3 <||>
(<||>) :: Parser a -> Parser a -> Parser a
a <||> b = try a <|> b

choice :: Foldable t => t (Parser a) -> Parser a
choice = foldr (<||>) empty

-- Math

sumUpTo :: Integral a => a -> a
sumUpTo n = n * (n + 1) `div` 2

fromBits :: (Foldable t, Num a) => t Bool -> a
fromBits = foldl' (\a b -> 2*a + if b then 1 else 0) 0

toBits :: Bits a => Int -> a -> [Bool]
toBits l n = [testBit n i | i <- [l-1,l-2..0]]

-- Lists and maps

(==>) = (,)

notNull :: Foldable t => t a -> Bool
notNull = not . null

lengthAtLeast, lengthAtMost :: Foldable t => t a -> Int -> Bool
lengthAtLeast t n = length (take n (toList t)) == n
lengthAtMost t n = length (take (n + 1) (toList t)) <= n

median :: [a] -> a
median l = l !! (length l `div` 2)

howMany :: (Num n, Foldable t) => (a -> Bool) -> t a -> n
howMany = howManyOf folded
howManyOf t p = foldlOf' t (\c e -> if p e then c + 1 else c) 0

counts :: (Num n, Foldable t, Ord a) => t a -> Map a n
counts = countsOf folded
countsOf t = foldlOf' t (\m e -> Map.insertWith (+) e 1 m) Map.empty

groups :: Ord k => [(k, a)] -> Map k [a]
groups kv = Map.fromListWith (++) [(k, [v]) | (k, v) <- kv]

rle :: Eq a => [a] -> [(a, Int)]
rle = map (head &&& length) . group

minimumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> a
minimumOn = minimumBy . comparing

maximumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> a
maximumOn = maximumBy . comparing

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

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = (x,) <$> xs <|> pairs xs

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

enumerate :: (Bounded a, Enum a) => [a]
enumerate = [minBound..maxBound]

fromTo :: (Ord a, Enum a) => a -> a -> [a]
fromTo from to | from <= to = [from..to]
               | otherwise  = [from,pred from..to]

alt :: (Foldable t, Alternative f) => t a -> f a
alt = alaf Alt foldMap pure

-- Functions and memoizing

fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint = fixedPointOn id

fixedPointOn :: Eq b => (a -> b) -> (a -> a) -> a -> a
fixedPointOn rep f x = go x (rep x) where
  go x rx | rx == rfx = fx
          | otherwise = go fx rfx
          where fx = f x
                rfx = rep fx

iterate1 :: (a -> a) -> a -> [a]
iterate1 f x = iterate f (f x)

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

-- not a valid Traversal, but useful
traverseSet :: (Applicative f, Ord b) => (a -> f b) -> Set a -> f (Set b)
traverseSet f = fmap Set.fromList . traverse f . Set.toList

-- 2D coordinates and grids

type Coords = (Integer, Integer)

instance Num i => Num (i, i) where
  (x1, y1) + (x2, y2) = (x1 + x2, y1 + y2)
  (x1, y1) * (x2, y2) = (x1 * x2, y1 * y2)
  negate (x, y) = (-x, -y)
  fromInteger n = (fromInteger n, fromInteger n)
  abs (x, y) = (abs x, abs y)
  signum = undefined

rectangle :: Coords -> (Integer, Integer) -> [Coords]
rectangle (x, y) (w, h) = [(i, j) | i <- [x..x + w - 1], j <- [y..y + h - 1]]

clamp :: Coords -> Coords -> Coords -> Coords
clamp (xmin, ymin) (xmax, ymax) (x, y) = (min xmax (max xmin x), min ymax (max ymin y))

mul :: Integer -> Coords -> Coords
mul n p = fromInteger n * p

ccw, cw :: Coords -> Coords
ccw (x, y) = (y, -x)
cw  (x, y) = (-y, x)

manhattan :: Coords -> Integer
manhattan (x, y) = abs x + abs y

up, right, down, left :: Num i => (i, i)
right = (1, 0)
down = (0, 1)
left = (-1, 0)
up = (0, -1)

cardinal :: Num i => [(i, i)]
cardinal = [up, right, down, left]

interCardinal :: Num i => [(i, i)]
interCardinal = [(-1, -1), (1, 1), (-1, 1), (1, -1)]

principal :: Num i => [(i, i)]
principal = cardinal ++ interCardinal

coordsAngle :: RealFloat n => Coords -> n
coordsAngle (x, y) = -atan2 (fromIntegral x) (fromIntegral y)

flattenWithCoords :: (Num n, Enum n) => [[a]] -> [((n, n), a)]
flattenWithCoords rows = [ ((x, y), a)
                         | (y, row) <- zip [0..] rows
                         , (x, a)   <- zip [0..] row
                         ]

makeGrid :: (Ord n, Enum n, Num n) => String -> (Map (n, n) Char, n, n)
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
  i = foldl1 (\(xmin, ymin) (x, y) -> (xmin `min` x, ymin `min` y)) c
  a = foldl1 (\(xmax, ymax) (x, y) -> (xmax `max` x, ymax `max` y)) c
  c = getCoords g

dimensions :: GridLike a => a -> (Integer, Integer)
dimensions g = (xmax - xmin + 1, ymax - ymin + 1) where
  ((xmin, ymin), (xmax, ymax)) = boundingBox g

rotateGrid :: GridLike a => a -> a
rotateGrid g = mapCoords (\(x, y) -> (y, width - x - 1)) g where
  (width, _) = dimensions g

transposeGrid :: GridLike a => a -> a
transposeGrid = mapCoords swap

withNeighbours :: (Ord i, Num i) => [i] -> (i -> a -> b) -> Map i a -> i -> [b]
withNeighbours ds f g p = [f p' a | p' <- map (p +) ds, Just a <- [g Map.!? p']]

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
