module Day20 where

import Debug.Trace
import Control.Lens
import Data.Map qualified as Map
import Data.Set qualified as Set

import AOC

data Tile = Tile Int (Map Coords Bool) deriving (Show, Eq, Ord)
type Board = Map Coords Tile

tileSize = 10

bounds g = ((minX, minY), (maxX, maxY)) where
    (minX, minY) = foldl1 (\(ix, iy) (x, y) -> (ix `min` x, iy `min` y)) (Map.keysSet g)
    (maxX, maxY) = foldl1 (\(ix, iy) (x, y) -> (ix `max` x, iy `max` y)) (Map.keysSet g)

dimensions g = (maxX - minX + 1, maxY - minY + 1) where
    ((minX, minY), (maxX, maxY)) = bounds g

gridToSet c = Map.keysSet . Map.filter (== c)

symmetries g = take 4 (iterate rotateGrid g) ++ take 4 (iterate rotateGrid (flipGrid g)) where

rotateGrid g = Map.mapKeys (\(x, y) -> (y, width - x - 1)) g where
    (width, _) = dimensions g

flipGrid = Map.mapKeys swap

edges g = [e1, e2, e3, e4] where
    (width, height) = dimensions g
    e1 = [g Map.! (i, 0)          | i <- [0..width-1]]
    e2 = [g Map.! (width-1, i) | i <- [0..height-1]]
    e3 = [g Map.! (i, height-1) | i <- reverse [0..width-1]]
    e4 = [g Map.! (0, i)          | i <- reverse [0..height-1]]

edges' t = take 4 $ drop 2 $ cycle $ edges t

insertTile board (Tile i g) = listToMaybe
    [ Map.insert (p + d) (Tile i g'') board
    | g'' <- symmetries g
    , (p, Tile _ g') <- Map.toList board
    , (e, e', d) <- zip3 (edges g') (edges' g'') cardinal
    , e == reverse e'
    ]

assemble (start:tiles) = go tiles [] (Map.singleton (0, 0) start) where
    go [] [] board = board
    go [] ts' board = go ts' [] board
    go (t:ts) ts' board | Just board' <- insertTile board t = go ts ts' board'
                        | otherwise = go ts (t:ts') board

paste = Map.foldMapWithKey context where
    context p (Tile _ g) = Map.mapKeys (\p' -> ((tileSize - 2) `mul` p) + p' - (1, 1)) $ Map.filterWithKey (\p' _ -> inRange ((1, 1), (tileSize-2, tileSize-2)) p') g

parseTiles = do
    n <- "Tile " *> decimal <* ":\n"
    (fmap (== '#') -> grid, _, _) <- makeGrid <$> many anySingle
    pure (Tile n grid)

main :: IO ()
main = do
    input <- filter notNull . splitOn "\n\n" <$> readInput
    tiles <- traverse (parseIO parseTiles) input
    let board = assemble tiles
        ((minX, minY), (maxX, maxY)) = bounds board
    print $ product [i | x <- [minX, maxX], y <- [minY, maxY], Just (Tile i _) <- [board Map.!? (x, y)]]
    let image = paste board
        (gridToSet '#' -> seaMonster, _, _) = makeGrid
            $ unlines ["                  # "
                      ,"#    ##    ##    ###"
                      ," #  #  #  #  #  #   "]
        findMonster image = length $ mconcat [monster | x <- [minX..maxX], y <- [minY..maxY], let monster = Set.map (+ (x, y)) seaMonster, all (\p -> Map.findWithDefault False p image) monster]
            where ((minX, minY), (maxX, maxY)) = bounds image
    print $ head [howMany id image - n | img <- symmetries image, let n = findMonster img, n /= 0]
