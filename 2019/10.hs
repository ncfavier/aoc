import Control.Arrow
import Data.List
import Data.Ord
import qualified Data.Map as M

parseAsteroids s = [(x, y) | (y, (x, '#')) <- sequenceA =<< zip [0..] (zip [0..] <$> lines s)]

angle (x, y) = -atan2 (fromIntegral x) (fromIntegral y)

manhattan (x, y) = abs x + abs y

insertAsteroid [x] xs = insertBy (comparing manhattan) x xs

nthAsteroid n asteroids = if n <= length rotation then M.elemAt (n - 1) rotation else nthAsteroid (n - length rotation) rest
    where
        (rotation, rest) = (M.map head &&& M.filter (not . null) . M.map tail) asteroids

main = do
    asteroids <- parseAsteroids <$> readFile "input10"
    let detects (x, y) = M.fromListWith insertAsteroid [(angle (x' - x, y' - y), [(x' - x, y' - y)]) | (x', y') <- asteroids, (x', y') /= (x, y)]
    let ((x, y), best) = maximumBy (comparing (length . snd)) (map (id &&& detects) asteroids)
    print $ length best
    let (_, (x', y')) = nthAsteroid 200 best
    print $ (x + x') * 100 + y + y'
