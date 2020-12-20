module Day20 where

import Data.Map qualified as Map
import Data.Set qualified as Set

import AOC

type Grid = Map Coords Bool
data Tile = Tile { tileId :: Int, tileGrid :: Grid }
type Board = Map Coords Tile

tileP :: Parser Tile
tileP = do
    n <- "Tile " *> decimal <* ":\n"
    (fmap (== '#') -> grid, _, _) <- makeGrid <$> many anySingle
    pure (Tile n grid)

symmetries :: Grid -> [Grid]
symmetries g = rotations g ++ rotations (flipGrid g) where
    rotations = take 4 . iterate rotateGrid

edges :: Grid -> [[Bool]]
edges g = transpose [(g Map.!) <$> [(i, 0), (m, i), (m - i, m), (0, m - i)] | i <- [0..m]] where
    (pred -> m, _) = dimensions g

insertTile :: Tile -> Board -> Maybe Board
insertTile (Tile i g') board = listToMaybe
    [ Map.insert (p + d) (Tile i g'') board
    | g'' <- symmetries g'
    , (p, Tile _ g) <- Map.toList board
    , (e, e', d) <- zip3 (edges g) (lineUp (edges g'')) cardinal
    , e == e'
    ]
    where lineUp [e1, e2, e3, e4] = reverse <$> [e3, e4, e1, e2]

assemble :: [Tile] -> Board
assemble (t:ts) = go ts [] (Map.singleton (0, 0) t) where
    go [] [] board                          = board
    go [] ts board                          = go ts [] board
    go (t:ts) ts' board
        | Just board' <- insertTile t board = go ts ts' board'
        | otherwise                         = go ts (t:ts') board

contextualise :: Coords -> Tile -> Grid
contextualise p (Tile _ g) = Map.fromList
    [ (size `mul` p + p' - 1, b)
    | (p', b) <- Map.toList g
    , inRange ((1, 1), (size, size)) p'
    ]
    where (subtract 2 -> size, _) = dimensions g

findPattern :: Set Coords -> Grid -> Set Coords
findPattern pat image = mconcat
    [ occurrence
    | (x, y) <- range (minImg, maxImg - (width, height) + 1)
    , let occurrence = Set.map (+ (x, y)) pat
    , all (\p -> Map.findWithDefault False p image) occurrence
    ]
    where (width, height) = dimensions pat
          (minImg, maxImg) = boundingBox image

main :: IO ()
main = do
    input <- filter notNull . splitOn "\n\n" <$> readInput
    tiles <- traverse (parseIO tileP) input
    let board = assemble tiles
        ((ix, iy), (ax, ay)) = boundingBox board
    print $ product [tileId (board Map.! (x, y)) | x <- [ix, ax], y <- [iy, ay]]
    let image = Map.foldMapWithKey contextualise board
        (gridToSet (== '#') -> seaMonster, _, _) = makeGrid
            $ unlines ["                  # "
                      ,"#    ##    ##    ###"
                      ," #  #  #  #  #  #   "]
    print $ howMany id image - head [length monsters | monsters <- findPattern seaMonster <$> symmetries image
                                                     , notNull monsters]
