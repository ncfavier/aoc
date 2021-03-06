module Day22 where

import Data.HashSet qualified as Set
import Safe.Exact

import AOC

handsP :: Parser ([Int], [Int])
handsP = do
    "Player 1:\n"
    p1 <- decimal `endBy` newline
    newline
    "Player 2:\n"
    p2 <- decimal `endBy` newline
    pure (p1, p2)

game :: Bool -> ([Int], [Int]) -> (Bool, [Int])
game recursive = go Set.empty where
    go seen s@(p1, _) | s `Set.member` seen = (True, p1)
    go _ (p1, []) = (True,  p1)
    go _ ([], p2) = (False, p2)
    go seen s@(x1:p1, x2:p2)
        | leftWins  = go seen' (p1 ++ [x1, x2], p2)
        | otherwise = go seen' (p1, p2 ++ [x2, x1])
        where seen' = Set.insert s seen
              leftWins | recursive
                       , Just p1' <- takeExactMay x1 p1
                       , Just p2' <- takeExactMay x2 p2 = fst (game recursive (p1', p2'))
                       | otherwise                      = x1 > x2

score :: [Int] -> Int
score p = sum $ zipWith (*) [1..] (reverse p)

main :: IO ()
main = do
    s <- parseInput handsP
    for_ [False, True] \recursive ->
        print $ score $ snd $ game recursive s
