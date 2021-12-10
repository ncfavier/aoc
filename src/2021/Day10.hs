module Day10 where

import AOC hiding (parse)

brackets = [('(',')'), ('[',']'), ('{','}'), ('<','>')]

parse :: String -> Either Char String
parse = go []
  where
    go stack (c:cs)
      | c `elem` "([{<" = go (c:stack) cs
      | otherwise = case stack of
        top:rest | (top, c) `elem` brackets -> go rest cs
        _ -> Left c
    go stack [] = Right stack

score1 :: Char -> Int
score1 ')' = 3
score1 ']' = 57
score1 '}' = 1197
score1 '>' = 25137

score2 :: String -> Int
score2 = foldl' (\a c -> 5 * a + s c) 0
  where
    s '(' = 1
    s '[' = 2
    s '{' = 3
    s '<' = 4

main :: IO ()
main = do
  (invalid, valid) <- partitionEithers . map parse . lines <$> readInput
  print $ sum $ score1 <$> invalid
  print $ median $ sort $ score2 <$> valid
