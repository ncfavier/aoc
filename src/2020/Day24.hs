module Day24 where

import Data.Map qualified as Map

import AOC

tileP :: Parser [Coords]
tileP = many (choice dirs) where
    dirs = [ ( 1,  0) <$ "e"
           , (-1,  0) <$ "w"
           , ( 0, -1) <$ "ne"
           , (-1, -1) <$ "nw"
           , ( 0,  1) <$ "sw"
           , ( 1,  1) <$ "se"
           ]

neighbours :: Coords -> [Coords]
neighbours p = map (p +) [(0, 0), (1,0), (-1,0), (0, -1), (-1, -1), (0, 1), (1, 1)]

rule :: Cell -> Bool
rule (Cell True n) | inRange (1,2) n = True
rule (Cell False 2)                  = True
rule _                               = False

main :: IO ()
main = do
    tiles <- parseInputLines tileP
    let alive = gridToSet id $ Map.fromListWith xor [(sum t, True) | t <- tiles]
    print $ length alive
    print $ length $ iterate (evolve neighbours rule) alive !! 100
