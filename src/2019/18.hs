module Day18 where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import AOC

left, right, up, down :: Coords -> Coords
left  = (pred *** id)
right = (succ *** id)
up    = (id *** pred)
down  = (id *** succ)

neighbours :: Coords -> [Coords]
neighbours p = [d p | d <- [left, right, up, down]]

main = do
    grid <- M.fromList . flatten . lines <$> getContents
    let m ! p = M.findWithDefault '.' p m
        nKeys = length $ M.filter isLower grid
        center = head [p | (p, '@') <- M.assocs grid]
        next (p, ks) = do
            n <- neighbours p
            case grid ! n of
                '.' -> return (n, ks)
                '@' -> return (n, ks)
                k | isLower k -> return (n, S.insert k ks)
                d | isUpper d, toLower d `S.member` ks -> return (n, ks)
                _ -> []
    print $ head [d | ((_, ks), d) <- bfs next (center, S.empty), length ks == nKeys]
    let grid' = M.union (M.fromList $ (center, '#'):[(n, '#') | n <- neighbours center]) grid
        start4 = [left (up center), left (down center), right (down center), right (up center)]
        next4 (l, ps, ks) = do
            (p, ps') <- case l of
                Just p -> return (p, ps)
                Nothing -> [(p, delete p ps) | p <- ps]
            n <- neighbours p
            case grid' ! n of
                '.' -> return (Just n, ps', ks)
                '@' -> return (Just n, ps', ks)
                k | isLower k -> return (Nothing, n:ps', S.insert k ks)
                d | isUpper d, toLower d `S.member` ks -> return (Just n, ps', ks)
                _ -> []
    print $ head [d | ((_, _, ks), d) <- bfs next4 (Nothing, start4, S.empty), length ks == nKeys]
