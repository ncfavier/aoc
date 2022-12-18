module Day20 where

import Data.Map (Map)
import qualified Data.Map as M

import AOC

neighbours :: Coords -> [Coords]
neighbours p = [d + p | d <- [left, right, up, down]]

main :: IO ()
main = do
    input <- lines <$> readInput
    let (width, height) = (genericLength (head input), genericLength input)
        distanceToEdge (x, y) = minimum [x, width - x, y, height - y]
        grid = M.fromList $ flattenWithCoords input
        tile p = M.findWithDefault ' ' p grid
        portal p | isUpper (tile (up + p))    = Just [tile (up + up + p), tile (up + p)]
                 | isUpper (tile (down + p))  = Just [tile (down + p), tile (down + down + p)]
                 | isUpper (tile (left + p))  = Just [tile (left + left + p), tile (left + p)]
                 | isUpper (tile (right + p)) = Just [tile (right + p), tile (right + right + p)]
                 | otherwise                = Nothing
        portals = M.fromListWith (++) [(n, [p]) | (p, '.') <- M.toList grid, Just n <- [portal p]]
        links = M.fromList $ do
            (n, pair@[_, _]) <- M.toList portals
            let [outer, inner] = sortOn distanceToEdge pair
            [(inner, (outer, 1)), (outer, (inner, -1))]
        [start] = portals M.! "AA"
        [end]   = portals M.! "ZZ"
        step p = [n | n <- neighbours p, tile n == '.'] ++
                 [n | Just (n, _) <- [links M.!? p]]
        stepLayered (p, l) = [(n, l ) | n <- neighbours p, tile n == '.'] ++
                             [(n, l') | Just (n, d) <- [links M.!? p], let l' = l + d, l' >= 0]
    print $ head [d | (p, d) <- bfs step [start], p == end]
    print $ head [d | ((p, 0), d) <- bfs stepLayered [(start, 0)], p == end]
