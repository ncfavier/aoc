module Day17 where

import Data.Set qualified as Set

import AOC

type Pos = [Int]

neighbours :: Pos -> [Pos]
neighbours = traverse (traverse (+) [-1..1])

rule :: Cell -> Bool
rule (Cell True n) | inRange (2, 3) n = True
rule (Cell False 3)                   = True
rule _                                = False

main :: IO ()
main = do
    (grid, _, _) <- makeGrid <$> readInput
    let alive = gridToSet (== '#') grid
        dim n = Set.map (\(x, y) -> replicate (n - 2) 0 ++ map fromInteger [x, y]) alive
    for_ [3, 4] \n ->
        print $ length $ iterate (evolve neighbours rule) (dim n) !! 6
