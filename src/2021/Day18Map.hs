{-# OPTIONS_GHC -Wno-missing-methods #-}
module Day18 where

import AOC
import Data.Map qualified as M

data LR = L | R deriving (Eq, Ord, Enum)
type Snailfish = Map [LR] Int

instance Num Snailfish where
  a + b = final (pair a b)

pair :: Snailfish -> Snailfish -> Snailfish
pair a b = M.mapKeys (L:) a <> M.mapKeys (R:) b

magnitude :: Snailfish -> Int
magnitude = alaf Sum (M.foldMapWithKey . curry) (uncurry $ flip $ foldr \case L -> (3*); R -> (2*))

snailfish :: Parser Snailfish
snailfish = single <|> pairP
  where
    single = M.singleton [] <$> decimal
    pairP = pair <$ "[" <*> snailfish <* "," <*> snailfish <* "]"

reduce :: Snailfish -> Maybe Snailfish
reduce x
  -- explode
  | (l, a):(r, b):_ <- [(k, v) | (k, v) <- M.assocs x, length k > 4]
  , let (xl, xr) = (M.takeWhileAntitone (< l) x, M.dropWhileAntitone (<= r) x)
  = Just $ M.updateMax (Just . (+ a)) xl <> M.singleton (init l) 0 <> M.updateMin (Just . (+ b)) xr
  -- split
  | (p, n):_ <- M.assocs (M.filter (>= 10) x)
  = Just $ M.insert (p ++ [L]) (n `div` 2)
         $ M.insert (p ++ [R]) ((n + 1) `div` 2)
         $ M.delete p x
reduce _ = Nothing

final :: Snailfish -> Snailfish
final x = maybe x final (reduce x)

main :: IO ()
main = do
  numbers <- parseInput (eachLine snailfish)
  print $ magnitude $ foldl1 (+) numbers
  print $ maximum [magnitude (a + b) | a <- numbers, b <- numbers, a /= b]
