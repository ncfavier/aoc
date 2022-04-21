module Day22 where

import AOC
import Data.Map qualified as M
import Data.Set qualified as S

format = eachLine do
  b <- True <$ "on" <||> False <$ "off"
  x <- (,) <$ " x=" <*> number <* ".." <*> number <* ","
  y <- (,) <$ "y=" <*> number <* ".." <*> number <* ","
  z <- (,) <$ "z=" <*> number <* ".." <*> number
  pure (b, x, y, z)

main :: IO ()
main = do
  input <- parseInput format
  let f s (b,x,y,z) | b = s <> cube
                    | otherwise = s S.\\ cube
                    where cube = S.fromList $ (,,) <$> range (clamp x) <*> range (clamp y) <*> range (clamp z)
      clamp (a,b) = (max a (-50), min b 50)
      intersect1 (x,y) (a,b) = max 0 $ min y b - max x a
      intersect3 (x,y,z) (a,b,c) = intersect1 x a * intersect1 y b * intersect1 z c
  print $ length $ foldl' f S.empty input
  -- TODO part 2
