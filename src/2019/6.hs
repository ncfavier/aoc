module Day6 where

import Control.Arrow
import qualified Data.Map as M

main = do
    edges <- map ((id *** tail) . break (== ')')) . lines <$> getContents
    let orbits = M.fromListWith (++) [(a, [b]) | (a, b) <- edges]
        f n o = n + sum (map (f (n + 1)) (M.findWithDefault [] o orbits))
    print $ f 0 "COM"
    let parent = M.fromList [(b, a) | (a, b) <- edges]
        pathToRoot n | Just p <- parent M.!? n = n:pathToRoot p
                     | otherwise               = []
        you = reverse $ pathToRoot "YOU"
        san = reverse $ pathToRoot "SAN"
        l = length $ takeWhile (uncurry (==)) $ zip you san
    print $ length you + length san - 2*l - 2
