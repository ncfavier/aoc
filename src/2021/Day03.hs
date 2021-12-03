module Day03 where

import AOC

import Data.Map qualified as M

fromBits :: (Foldable t, Num a) => t Bool -> a
fromBits = foldl' (\a b -> 2*a + if b then 1 else 0) 0

mostCommon :: Foldable t => t Bool -> Bool
mostCommon = counts >>> \m -> m M.! True >= m M.! False

search :: Bool -> [[Bool]] -> [Bool]
search criterion xs = head [x | [x] <- scanl sieve xs [0..]]
  where
    sieve xs n = filter (\x -> x !! n == target) xs
      where
        target = criterion `xor` mostCommon (map (!! n) xs)

main :: IO ()
main = do
  report <- map (map (== '1')) . lines <$> readInput
  let gamma = mostCommon <$> transpose report
      epsilon = not <$> gamma
      oxygen = search False report
      co2    = search True report
  print (fromBits gamma * fromBits epsilon)
  print (fromBits oxygen * fromBits co2)
