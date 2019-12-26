{-# LANGUAGE TupleSections, MultiWayIf #-}
module Day24 where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import AOC

type Tile = (Integer, Coords)

size :: Integer
size = 5

center :: Coords
center = (size `div` 2, size `div` 2)

inBounds :: Coords -> Bool
inBounds (x, y) = x >= 0 && x < size && y >= 0 && y < size

neighbours :: Tile -> [Tile]
neighbours (l, p) = [ (l, p')
                    | d <- [left, right, up, down]
                    , let p' = d p
                    , inBounds p' ]

recursiveNeighbours :: Tile -> [Tile]
recursiveNeighbours (l, p) = do
    (d, border) <- [(left, (pred size,)), (up, (,pred size)), (right, (0,)), (down, (,0))]
    let p' = d p
    if | p' == center -> [(succ l, border i) | i <- [0..pred size]]
       | inBounds p'  -> [(l, p')]
       | otherwise    -> [(pred l, d center)]

interactWith :: (Tile -> [Tile]) -> Set Tile -> Set Tile
interactWith neighbours bugs = (keysWhere (== 1) $ n `M.restrictKeys` bugs) `S.union`
                               (keysWhere (`elem` [1, 2]) $ n `M.withoutKeys` bugs)
    where n = M.fromListWith (+) [(p, 1) | p <- foldMap neighbours bugs]
          keysWhere p = M.keysSet . M.filter p

biodiversityRating :: Set Tile -> Integer
biodiversityRating = sum . map biodiversity . S.toList
    where biodiversity (_, (x, y)) = 2 ^ (x + y * size)

main :: IO ()
main = do
    input <- flatten . lines <$> readInput
    let bugs = S.fromList [(0, p) | (p, '#') <- input]
    print $ firstDuplicate $ biodiversityRating <$> iterate (interactWith neighbours) bugs
    print $ length $ iterate (interactWith recursiveNeighbours) bugs !! 200
