module Day18 where

import AOC

plus, times :: Operator Parser Integer
plus  = InfixL ((+) <$ lexeme "+")
times = InfixL ((*) <$ lexeme "*")

expr :: [[Operator Parser Integer]] -> Parser Integer
expr ops = go where
    go   = makeExprParser base ops
    base = lexeme decimal <|> between (lexeme "(") (lexeme ")") go

main :: IO ()
main = do
    input <- readInput
    for_ [[[plus, times]], [[plus], [times]]] \ops ->
        print . sum =<< parseIOLines (expr ops) input
