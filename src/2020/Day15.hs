module Day15 where

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import AOC

run :: Int -> [Int] -> Int
run n = go 0 Nothing IM.empty where
    go i (Just l) _ | i == n = const l
    go i ml !seen = \case
        (x:xs) -> go (i + 1) (Just x) seen' xs
        []     -> go (i + 1) (Just c) seen' []
        where seen' | Just l <- ml = IM.insert l (i - 1) seen
                    | otherwise    = seen
              c | Just l <- ml, Just li <- seen IM.!? l = i - 1 - li
                | otherwise                             = 0

main :: IO ()
main = do
    input <- parseInput (number `sepBy` "," <* newline)
    for_ [2020, 30000000] \n -> print (run n input)
