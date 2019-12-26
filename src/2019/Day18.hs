module Day18 where

import Data.Array.IArray as A
import Data.Array.Unboxed
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import AOC

neighbours :: Coords -> [Coords]
neighbours p = [d p | d <- [left, right, up, down]]

diagonal :: Coords -> [Coords]
diagonal p = [d p | d <- [left . up, left . down, right . up, right . down]]

shortestPath :: UArray Coords Char -> Integer
shortestPath grid = head [d | ((ps, ks), d) <- dijkstra bigStep (start, S.empty)
                            , length ks == length keys]
    where
        keys = [(p, c) | (p, c) <- A.assocs grid, isLower c]
        start = sort [p | (p, '@') <- A.assocs grid]
        smallStep (p, ks) = do
            n <- neighbours p
            case grid ! n of
                '#' -> []
                d | isUpper d -> return (n, S.insert (toLower d) ks)
                _ -> return (n, ks)
        distancesToKeys = M.fromList [(p, dists p) | p <- start ++ map fst keys]
            where dists start = [ (k, p, d, ds)
                                | ((p, ds), d) <- bfsOn fst smallStep (start, S.empty)
                                , let k = grid ! p
                                , isLower k ]
        bigStep (ps, ks) = [ ((insert p' ps', S.insert k ks), d)
                           | (p, ps') <- pickOne ps
                           , (k, p', d, ds) <- distancesToKeys M.! p
                           , k `S.notMember` ks
                           , ds `S.isSubsetOf` ks ]

main :: IO ()
main = do
    input <- lines <$> readInput
    let (width, height) = (genericLength (head input), genericLength input)
        center = (width `div` 2, height `div` 2)
        grid = array ((0, 0), (width - 1, height - 1)) (flatten input)
    print (shortestPath grid)
    let grid' = grid // [(p, '#') | p <- center:neighbours center]
                     // [(p, '@') | p <- diagonal center]
    print (shortestPath grid')
