module Day03 where

import           Data.Map (Map)
import qualified Data.Map as Map

import AOC

main = do
    input <- readInput
    let (grid, width, height) = makeGrid input
        countTrees slope = count (== '#') (go (0, 0)) where
            go p@(x, y) | y >= height = []
                        | otherwise   = grid Map.! (x `mod` width, y):go (p `add` slope)
    print $ countTrees (3, 1)
    print $ product $ map countTrees [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
