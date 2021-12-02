module Day02 where

import AOC

data Command = Forward | Down | Up

format = eachLine do
  c <- Forward <$ "forward" <|> Down <$ "down" <|> Up <$ "up"
  n <- space *> number
  pure (c, n)

run1 Forward n = _2 += n
run1 Down    n = _1 += n
run1 Up      n = _1 -= n

run2 Forward n = do
  _2 += n
  aim <- use _3
  _1 += aim * n
run2 Down n = _3 += n
run2 Up   n = _3 -= n

solve run start cs = evalState ?? start $ do
  traverse_ (uncurry run) cs
  (*) <$> use _1 <*> use _2

main :: IO ()
main = do
  commands <- parseInput format
  print $ solve run1 (0, 0) commands
  print $ solve run2 (0, 0, 0) commands
