{-# OPTIONS_GHC -Wno-missing-methods #-}
module Day18 where

import AOC

data Snailfish = Pure Int | Pair Snailfish Snailfish deriving (Eq)

magnitude (Pure n) = n
magnitude (Pair a b) = 3 * magnitude a + 2 * magnitude b

snailfish = single <|> pair
  where
    single = Pure <$> decimal
    pair = Pair <$ "[" <*> snailfish <* "," <*> snailfish <* "]"

explode x = fmap (\(_, x, _) -> x) (go 0 x)
  where
    go d (Pair (Pure a) (Pure b)) | d >= 4 = Just (rightmost (+a), Pure 0, leftmost (+b))
    go d (Pair a b) = (\(f, x, g) -> (f , Pair x (g b), id)) <$> go (d+1) a
                  <|> (\(f, x, g) -> (id, Pair (f a) x, g )) <$> go (d+1) b
    go _ _ = Nothing
    leftmost f (Pure n) = Pure (f n)
    leftmost f (Pair a b) = Pair (leftmost f a) b
    rightmost f (Pure n) = Pure (f n)
    rightmost f (Pair a b) = Pair a (rightmost f b)

split (Pure n) | n >= 10 = Just $ Pair (Pure $ n `div` 2) (Pure $ (n + 1) `div` 2)
               | otherwise = Nothing
split (Pair a b) = flip Pair b <$> split a <|> Pair a <$> split b

reduce x = explode x <|> split x

final x = maybe x final $ reduce x

instance Num Snailfish where a + b = final (Pair a b)

main :: IO ()
main = do
  numbers <- parseInput (eachLine snailfish)
  print $ magnitude $ foldl1 (+) numbers
  print $ maximum [magnitude (a + b) | a <- numbers, b <- numbers, a /= b]
