module Day20 where

import AOC

import Data.CircularList

main :: IO ()
main = do
  input <- parseInput $ eachLine number
  let l1 = fromList $ zip [0..] input
      l2 = l1 & mapped._2 *~ 811589153
      n = length input
      move l i = insertL (i, f) $ rotN (f `rem` pred n) $ removeL l'
        where
          Just l' = findRotateTo ((i ==) . fst) l
          Just (_, f) = focus l'
      mix l = foldl' move l [0..pred n]
      part1 = mix l1
      part2 = iterate mix l2 !! 10
      extract = sum . catMaybes . map focus . take 3 . iterate1 (rotN 1000) . fromJust . rotateTo 0 . fmap snd
  print $ extract part1
  print $ extract part2
