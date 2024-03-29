module Day10 where

import qualified Data.Map as M

import AOC

parseAsteroids g = [p | (p, '#') <- flattenWithCoords (lines g)]

insertAsteroid [x] = insertBy (comparing manhattan) x

nthAsteroid n asteroids | n <= r    = M.elemAt (n - 1) rotation
                        | otherwise = nthAsteroid (n - r) rest
    where (rotation, rest) = (M.map head &&& M.filter (not . null) . M.map tail) asteroids
          r = length rotation

main = do
    asteroids <- parseAsteroids <$> readInput
    let detects (x, y) = M.fromListWith insertAsteroid [(angle (x' - x, y' - y), [(x' - x, y' - y)]) | (x', y') <- asteroids, (x', y') /= (x, y)]
    let ((x, y), best) = maximumBy (comparing (length . snd)) (map (id &&& detects) asteroids)
    print $ length best
    let (_, (x', y')) = nthAsteroid 200 best
    print $ (x + x') * 100 + y + y'
