module Day10 where

import AOC

data Instruction = Noop | Addx Int deriving Show

format :: Parser [Instruction]
format = eachLine (noop <|> addx) where
  noop = Noop <$ "noop"
  addx = Addx <$ "addx " <*> number

cpu :: [Instruction] -> [Int]
cpu = go 1 where
  go x (Noop:is) = x:go x is
  go x (Addx n:is) = x:x:go (x + n) is
  go _ [] = []

signalStrength :: [Int] -> Int
signalStrength xs = sum [c * (xs !! pred c) | c <- cycles] where
  cycles = takeWhile (<= length xs) [20 + i * 40 | i <- [0..]]

crt :: [Int] -> [String]
crt xs = zipWith pixel [0..] <$> chunksOf 40 xs where
  pixel t x | abs (t - x) <= 1 = '#'
            | otherwise = ' '

main :: IO ()
main = do
  instructions <- parseInput format
  let xs = cpu instructions
  print (signalStrength xs)
  putStrLn `mapM_` crt xs
