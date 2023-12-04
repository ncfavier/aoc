module Day04 where

import AOC

import Data.Set qualified as S

format = eachLine do (,) <$ lexeme "Card" <* number <* lexeme ":" <*> (S.fromList <$> many (lexeme number)) <* lexeme "|" <*> many (lexeme number)

score 0 = 0
score n = 2 ^ pred n

main :: IO ()
main = do
  input <- parseInput format
  let counts = [howMany (`S.member` winning) have | (winning, have) <- input]
  print $ sum (map score counts)
  print $ sum $ löb [\c -> 1 + sum [c !! k | k <- [i+1..i+n]] | (i, n) <- zip [0..] counts]
