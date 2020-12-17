module Day17 where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import AOC

type Pos = [Int]

data Cell = Cell !Bool !Int

instance Semigroup Cell where
    Cell a1 n1 <> Cell a2 n2 = Cell (a1 || a2) (n1 + n2)

neighbours :: Pos -> [(Pos, Cell)]
neighbours p = [ (p', Cell self (1 - fromEnum self))
               | p' <- traverse (traverse (+) [-1..1]) p
               , let self = p' == p
               ]

rule :: Cell -> Bool
rule (Cell True n) | inRange (2, 3) n = True
rule (Cell False 3)                   = True
rule _                                = False

evolve :: Set Pos -> Set Pos
evolve alive = Map.keysSet (Map.filter rule cells) where
    cells = Map.fromListWith (<>) (foldMap neighbours alive)

main :: IO ()
main = do
    (grid, _, _) <- makeGrid <$> readInput
    let alive = Map.keysSet (Map.filter (== '#') grid)
        dim n = Set.map (\(x, y) -> replicate (n - 2) 0 ++ map fromInteger [x, y]) alive
    for_ [3, 4] \n ->
        print $ length $ iterate evolve (dim n) !! 6
