module AOC ( module AOC
           , module Control.Applicative
           , module Control.Arrow
           , module Control.Comonad.Store
           , module Control.Lens
           , module Control.Monad
           , module Control.Monad.Combinators.Expr
           , module Control.Monad.Search
           , module Control.Monad.State
           , module Data.Bits
           , module Data.Bool
           , module Data.Char
           , module Data.Composition
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
           , module Data.MemoTrie
           , module Data.Ord
           , module Data.Ratio
           , module Data.Semigroup
           , module Data.Set
           , module Data.Traversable
           , module Data.Tuple
           , module Data.Void
           , module GHC.Natural
           , module Linear
           , module Text.Megaparsec
           , module Text.Megaparsec.Char
           , module Text.Megaparsec.Char.Lexer
           , module Text.Read
           ) where

import Control.Applicative
import Control.Arrow hiding (left, right)
import Control.Comonad.Store
import Control.Lens hiding (noneOf)
import Control.Monad
import Control.Monad.Combinators.Expr
import Control.Monad.Search
import Control.Monad.State
import Data.Bits
import Data.Bool
import Data.Char
import Data.Composition
import Data.Either
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Functor.Identity
import Data.Ix hiding (index)
import Data.List hiding (uncons)
import Data.List.Split (splitOn, chunksOf, divvy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Strict qualified as MapS
import Data.Maybe
import Data.MemoTrie hiding (enumerate)
import Data.Monoid
import Data.Ord hiding (Down(..), clamp)
import Data.PriorityQueue.FingerTree qualified as PQ
import Data.Ratio
import Data.Semigroup
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable
import Data.Tuple
import Data.Void
import GHC.Natural
import Linear hiding (transpose, rotate)
import System.Environment
import System.Exit
import System.IO
import Text.Megaparsec hiding (State(..), Pos, choice, many, some, takeP, takeWhileP, takeWhile1P)
import Text.Megaparsec qualified
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (signed, binary, octal, decimal, hexadecimal)
import Text.Read (readMaybe)

instance (Ord c, Monoid c, Monad m) => MonadFail (SearchT c m) where fail _ = empty

-- IO and parsing

readInput :: IO String
readInput = do
  hSetBuffering stdout NoBuffering -- I don't know where else to put this
  maybe getContents readFile =<< lookupEnv "AOC_INPUT"

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

restOfLine :: Parser String
restOfLine = takeWhileP (/= '\n') <* newline

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

mod1 :: Integral a => a -> a -> a
a `mod1` n = (a - 1) `mod` n + 1

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

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (flip compare)

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

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f = go where
  go a = a:maybe [] go (f a)

nTimes :: (Eq n, Num n, Enum n) => (a -> a) -> n -> a -> a
nTimes _ 0 = id
nTimes f n = f . nTimes f (pred n)

nTimesCycle :: (Ord a, Integral n) => (a -> a) -> n -> a -> a
nTimesCycle f n = go Map.empty 0 where
  go seen k a
    | k == n = a
    | Just m <- Map.lookup a seen
    , Just (a', _) <- Map.lookupMin (Map.filter (== m + ((n - m) `mod` (k - m))) seen) = a'
    | otherwise = go (Map.insert a k seen) (succ k) (f a)

fixMem :: (Ord k, Foldable t) => t k -> ((k -> a) -> k -> a) -> k -> a
fixMem keys f = go where
  m = Map.fromList [(x, go x) | x <- toList keys]
  go = f \x -> Map.findWithDefault (go x) x m

mem :: (Ord k, Foldable t) => t k -> (k -> a) -> k -> a
mem keys f = \x -> Map.findWithDefault (f x) x m
  where m = Map.fromList [(x, f x) | x <- toList keys]

löb :: Functor f => f (f a -> a) -> f a
löb m = go where go = fmap ($ go) m

-- löb over lens (well, an indexed setter)
löl :: AnIndexedSetter i s t a b -> s -> (t -> i -> a -> b) -> t
löl l s f = go where
  go = s & l %@~ f go

-- not a valid Traversal, but useful
traverseSet :: (Applicative f, Ord b) => (a -> f b) -> Set a -> f (Set b)
traverseSet f = fmap Set.fromList . traverse f . Set.toList

insertMaybeSet :: Ord a => a -> Set a -> Maybe (Set a)
insertMaybeSet = Set.alterF (\p -> True <$ guard (not p))

insertMaybeMap :: Ord k => k -> v -> Map k v -> Maybe (Map k v)
insertMaybeMap k v = Map.alterF (\p -> Just v <$ guard (isNothing p)) k

-- Coordinates and grids

type Coords = (Integer, Integer)
type Coords3 = (Integer, Integer, Integer)

instance Num i => Num (i, i) where
  (x1, y1) + (x2, y2) = (x1 + x2, y1 + y2)
  (x1, y1) * (x2, y2) = (x1 * x2, y1 * y2)
  negate (x, y) = (-x, -y)
  fromInteger n = (fromInteger n, fromInteger n)
  abs (x, y) = (abs x, abs y)
  signum = undefined

instance Num i => Num (i, i, i) where
  (x1, y1, z1) + (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)
  (x1, y1, z1) * (x2, y2, z2) = (x1 * x2, y1 * y2, z1 * z2)
  negate (x, y, z) = (-x, -y, -z)
  fromInteger n = (fromInteger n, fromInteger n, fromInteger n)
  abs (x, y, z) = (abs x, abs y, abs z)
  signum = undefined

instance Semigroup Integer where
  (<>) = (+)
  stimes = stimesMonoid

instance Monoid Integer where
  mempty = 0

rectangle :: Coords -> (Integer, Integer) -> [Coords]
rectangle (x, y) (w, h) = [(i, j) | i <- [x..x + w - 1], j <- [y..y + h - 1]]

clamp :: Coords -> Coords -> Coords -> Coords
clamp (xmin, ymin) (xmax, ymax) (x, y) = (min xmax (max xmin x), min ymax (max ymin y))

clampMod :: Integral a => (a, a) -> a -> a
clampMod (a, b) n = a + (n - a) `mod` (b - a + 1)

mul :: Integer -> Coords -> Coords
n `mul` p = fromInteger n * p

ccw, cw :: Coords -> Coords
ccw (x, y) = (y, -x)
cw  (x, y) = (-y, x)

manhattan :: Coords -> Integer
manhattan (x, y) = abs x + abs y

origin, up, right, down, left :: Num i => (i, i)
origin = (0, 0)
right = (1, 0)
down = (0, 1)
left = (-1, 0)
up = (0, -1)

cardinal :: Num i => [(i, i)]
cardinal = [up, right, down, left]

cardinal3 :: Num i => [(i, i, i)]
cardinal3 = [(0, 0, -1), (0, 0, 1), (0, -1, 0), (0, 1, 0), (-1, 0, 0), (1, 0, 0)]

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

makeGrid :: (Ord n, Enum n, Num n) => String -> Map (n, n) Char
makeGrid s = grid
  where (grid, _, _) = makeGrid' s

makeGrid' :: (Ord n, Enum n, Num n) => String -> (Map (n, n) Char, n, n)
makeGrid' s = (grid, width, height) where
  rows = lines s
  grid = Map.fromList (flattenWithCoords rows)
  width = genericLength (head rows)
  height = genericLength rows

innerEdge :: (Ord n, Enum n, Num n) => n -> n -> Set (n, n)
innerEdge width height = Set.fromList $ concat $ [[(0, y), (width-1, y)] | y <- [0..height-1]] ++ [[(x, 0), (x, height-1)] | x <- [0..width-1]]

outerEdge :: (Ord n, Enum n, Num n) => n -> n -> Set (n, n)
outerEdge width height = Set.fromList $ concat $ [[(-1, y), (width, y)] | y <- [-1..height]] ++ [[(x, -1), (x, height)] | x <- [-1..width]]

mapToSet :: (a -> Bool) -> Map k a -> Set k
mapToSet p = Map.keysSet . Map.filter p

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

dfs :: (Num n, Ord a) => (a -> [a]) -> [a] -> [(a, n)]
dfs = dfsOn id

dfsOn :: (Num n, Ord b) => (a -> b) -> (a -> [a]) -> [a] -> [(a, n)]
dfsOn rep next start = go Set.empty (Seq.fromList $ (,0) <$> start) where
  go _ Seq.Empty = []
  go seen (ps Seq.:|> (n, d))
    | r `Set.member` seen = go seen ps
    | otherwise           = (n, d):go (Set.insert r seen) (ps <> Seq.fromList [(n', d + 1) | n' <- next n])
    where r = rep n

bfs :: (Num n, Ord a) => (a -> [a]) -> [a] -> [(a, n)]
bfs = bfsOn id

bfsOn :: (Num n, Ord b) => (a -> b) -> (a -> [a]) -> [a] -> [(a, n)]
bfsOn rep next start = go Set.empty (Seq.fromList $ (,0) <$> start) where
  go _ Seq.Empty = []
  go seen ((n, d) Seq.:<| ps)
    | r `Set.member` seen = go seen ps
    | otherwise           = (n, d):go (Set.insert r seen) (ps <> Seq.fromList [(n', d + 1) | n' <- next n])
    where r = rep n

dijkstra :: (Num n, Ord n, Ord a) => (a -> [(a, n)]) -> [a] -> [(a, n)]
dijkstra = dijkstraOn id

dijkstraOn :: (Num n, Ord n, Ord b) => (a -> b) -> (a -> [(a, n)]) -> [a] -> [(a, n)]
dijkstraOn rep next = astarOn rep next'
  where next' n = [(n', c, 0) | (n', c) <- next n]

astar :: (Num n, Ord n, Ord a) => (a -> [(a, n, n)]) -> [a] -> [(a, n)]
astar = astarOn id

astarOn :: (Num n, Ord n, Ord b) => (a -> b) -> (a -> [(a, n, n)]) -> [a] -> [(a, n)]
astarOn rep next start = go Set.empty (PQ.fromList $ map (\s -> (0, (0, s))) start) where
  go seen q
    | Just ((_, (d, n)), q') <- PQ.minViewWithKey q =
      let r = rep n
          insert q (n', c, h) = PQ.insert (d' + h) (d', n') q where d' = d + c
      in case insertMaybeSet r seen of
        Nothing -> go seen q'
        Just seen' -> (n, d):go seen' (foldl' insert q' (next n))
    | otherwise = []
