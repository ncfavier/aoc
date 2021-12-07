module Day07 where

import AOC

constantFuel, increasingFuel :: Int -> Int
constantFuel   d = d
increasingFuel d = d * (d + 1) `div` 2

main :: IO ()
main = do
  crabs <- parseInput (number `sepBy` "," <* newline)
  for_ [constantFuel, increasingFuel] \fuel ->
    print $ minimum [ sum [fuel (abs (crab - meet)) | crab <- crabs]
                    | meet <- [minimum crabs..maximum crabs]
                    ]
