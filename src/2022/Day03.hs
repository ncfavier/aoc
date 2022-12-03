module Day03 where

import AOC

import Data.List.Split
import Data.Map qualified as M
import Data.Set qualified as S

format = eachLine takeRest

priority c | isAsciiLower c = fromEnum c - fromEnum 'a' + 1
           | isAsciiUpper c = fromEnum c - fromEnum 'A' + 27

part1 rucksack = priority common
  where
    (a, b) = splitAt (length rucksack `div` 2) rucksack
    Just common = S.lookupMin $ S.fromList a `S.intersection` S.fromList b

groupSize = 3

part2 group = priority common
  where
    Just (common, _) = M.lookupMin $ M.filter (== groupSize) $ counts $ concatMap nub group

main :: IO ()
main = do
  rucksacks <- parseInput format
  print $ sum $ map part1 rucksacks
  print $ sum $ map part2 $ chunksOf groupSize rucksacks
