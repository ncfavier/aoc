module Day02 where

import AOC

data Command = Forward | Down | Up

format = eachLine do
  d <- Forward <$ "forward" <|> Down <$ "down" <|> Up <$ "up"
  " "
  n <- number
  pure (d, n)

run1 Forward n = _2 += n
run1 Down n = _1 += n
run1 Up n = _1 -= n

run2 Forward n = do
  _2 += n
  a <- use _3
  _1 += a * n
run2 Down n = _3 += n
run2 Up n = _3 -= n

solve run start cs = evalState ?? start $ do
  traverse_ (uncurry run) cs
  y <- use _1
  x <- use _2
  pure (x * y)

main :: IO ()
main = do
  cs <- parseInput format
  print $ solve run1 (0, 0) cs
  print $ solve run2 (0, 0, 0) cs
