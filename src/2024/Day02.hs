module Day02 where

import AOC

format = eachLine do many (lexeme number)

safe :: [Int] -> Bool
safe ns = allEqual (signum <$> ns') && all (inRange (1, 3)) (abs <$> ns')
  where
    ns' = (zipWith subtract <*> tail) ns

safe' :: [Int] -> Bool
safe' ns = any safe (snd <$> pickOne ns)

main = do
  input <- parseInput format
  print $ howMany safe input
  print $ howMany safe' input
