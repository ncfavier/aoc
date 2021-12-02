module Day02 where

import AOC

data Command = Forward | Down | Up

format = eachLine do
  c <- Forward <$ "forward" <|> Down <$ "down" <|> Up <$ "up"
  n <- space *> number
  pure (c, n)

run1 Forward n = _x += n
run1 Down    n = _y += n
run1 Up      n = _y -= n

run2 Forward n = do
  _x += n
  aim <- use _z
  _y += aim * n
run2 Down n = _z += n
run2 Up   n = _z -= n

solve run start cs = evalState ?? start $ do
  traverse_ (uncurry run) cs
  product <$> use _xy

main :: IO ()
main = do
  commands <- parseInput format
  print $ solve run1 (V2 0 0  ) commands
  print $ solve run2 (V3 0 0 0) commands
