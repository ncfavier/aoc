module Day3 where

import Control.Arrow
import qualified Data.Map as M

split :: Char -> String -> [String]
split d = words . map (\c -> if c == d then ' ' else c)

readDirections :: String -> [((Int, Int), Int)]
readDirections = map (\(c:n) -> (direction c, read n)) . split ','

(+:) :: Num a => (a, a) -> (a, a) -> (a, a)
(a1, b1) +: (a2, b2) = (a1 + a2, b1 + b2)

norm :: Num a => (a, a) -> a
norm (a, b) = abs a + abs b

direction :: Char -> (Int, Int)
direction 'L' = (-1, 0)
direction 'R' = (1, 0)
direction 'D' = (0, -1)
direction 'U' = (0, 1)

path _ [] = []
path o ((d, n):xs) = p ++ path (last p) xs
    where p = take n $ tail $ iterate ((+: d) *** succ) o

main :: IO ()
main = do
    [w1, w2] <- map readDirections . lines <$> getContents
    let [s1, s2] = map (M.fromListWith min . path ((0, 0), 0)) [w1, w2]
        i = M.intersectionWith (+) s1 s2
    print $ minimum $ map norm $ M.keys i
    print $ minimum $ M.elems i
