module Day22 where

import AOC

import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Set qualified as S

format = S.fromList <$> eachLine do brick <$> coords <* "~" <*> coords where
  coords = (,,) <$> number <* "," <*> number <* "," <*> number
  brick (a, b, c) (d, e, f) = ((min a d, max a d), (min b e, max b e), (min c f, max c f))

(x, y, z) << (x', y', z') = overlap x x' && overlap y y' && (z <<< z') where
  overlap a b = not $ a <<< b || b <<< a
  (_, b) <<< (c, _) = b < c

fall ground (x, y, (z, z')) = (x, y, (ground + 1, z' - z + ground + 1))

main = do
  bricks <- parseInput format
  let
    supportedBy = fmap fst $ löb $ bricks & M.fromSet \ b s ->
      let
        supportedBy = maximums [Arg z b' | (b', ~(_, (_, _, (_, z)))) <- M.assocs s, b' << b]
        ground | Arg z _:_ <- supportedBy = z
               | otherwise = 0
      in ((\(Arg _ b) -> b) <$> supportedBy, fall ground b)
    supportsTrans = bricks & M.fromSet \ b ->
      howMany id $ M.delete b $ löb $ M.insert b (\_ -> True) $ supportedBy <&> \ bs s ->
        notNull bs && all (s M.!) bs
  print $ howMany (== 0) supportsTrans
  print $ sum supportsTrans
