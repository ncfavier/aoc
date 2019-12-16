module Day10 where

import Control.Arrow
import Data.List
import Data.Ord
import qualified Data.Map as M

parseAsteroids s = [(x, y) | (y, (x, '#')) <- sequenceA =<< zip [0..] (zip [0..] <$> lines s)]

angle (x, y) = -atan2 (fromIntegral x) (fromIntegral y)

manhattan (x, y) = abs x + abs y

insertAsteroid [x] = insertBy (comparing manhattan) x

nthAsteroid n asteroids | n <= r    = M.elemAt (n - 1) rotation
                        | otherwise = nthAsteroid (n - r) rest
    where (rotation, rest) = (M.map head &&& M.filter (not . null) . M.map tail) asteroids
          r = length rotation

main = do
    asteroids <- parseAsteroids <$> getContents
    let detects (x, y) = M.fromListWith insertAsteroid [(angle (x' - x, y' - y), [(x' - x, y' - y)]) | (x', y') <- asteroids, (x', y') /= (x, y)]
    let ((x, y), best) = maximumBy (comparing (length . snd)) (map (id &&& detects) asteroids)
    print $ length best
    let (_, (x', y')) = nthAsteroid 200 best
    print $ (x + x') * 100 + y + y'
