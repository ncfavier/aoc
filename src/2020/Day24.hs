module Day24 where

import Data.Map qualified as Map

import AOC

hexDirs :: [Coords]
hexDirs = [(-1, 0), (1, 0), (-1, -1), (0, -1), (0, 1), (1, 1)]

tileP :: Parser Coords
tileP = sum <$> many (choice dirP) where
    dirP = zipWith (<$) hexDirs ["w", "e", "nw", "ne", "sw", "se"]

neighbours :: Coords -> [Coords]
neighbours = traverse (+) ((0, 0):hexDirs)

rule :: Cell -> Bool
rule (Cell True n) | inRange (1,2) n = True
rule (Cell False 2)                  = True
rule _                               = False

main :: IO ()
main = do
    tiles <- parseInput $ eachLine tileP
    let alive = mapToSet id $ Map.fromListWith xor [(t, True) | t <- tiles]
    print $ length alive
    print $ length $ iterate (evolve neighbours rule) alive !! 100
