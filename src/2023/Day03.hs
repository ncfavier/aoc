module Day03 where

import AOC

import Data.Set qualified as S

main :: IO ()
main = do
  input <- readInput
  let grid = makeGrid input
  let symbols = mapToSet (\c -> c /= '.' && not (isDigit c)) grid
      symbols' = S.unions (symbols:[S.map (+ d) symbols | d <- principal])
  let numbers = [ (read (map snd r) :: Int, S.fromList (map (i <$) r))
                | (i, l) <- zip [0..] (lines input)
                , r <- groupBy ((==) `on` isDigit . snd) (zip [0..] l)
                , isDigit (snd (head r))]
  print $ sum [n | (n, r) <- numbers, notNull (r `S.intersection` symbols')]
  let gears = mapToSet (== '*') grid
  print $ sum [a * b | g <- toList gears, [a, b] <- pure [n | (n, ts) <- numbers, g `S.member` (S.unions (ts:[S.map (+ d) ts | d <- principal]))]]
