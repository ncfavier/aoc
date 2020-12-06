module Day06 where

import           Data.Set (Set)
import qualified Data.Set as Set

import AOC

main = do
    groups <- splitOn "\n\n" <$> readInput
    for [Set.unions, foldr1 Set.intersection] \f ->
        print $ sum $ map (length . f . map Set.fromList . lines) groups
