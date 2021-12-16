module Day03 where

import AOC

mostCommon :: Foldable t => t Bool -> Bool
mostCommon t = howMany (== True) t >= howMany (== False) t

search :: Bool -> [[Bool]] -> [Bool]
search criterion xs = head [x | [x] <- scanl sieve xs [0..]]
  where
    sieve xs n = filter (\x -> x !! n == target) xs
      where
        target = criterion `xor` mostCommon (map (!! n) xs)

main :: IO ()
main = do
  report <- map (map (== '1')) . lines <$> readInput
  let γ = mostCommon <$> transpose report
      ε = not <$> γ
      o₂  = search False report
      co₂ = search True report
  print (fromBits γ * fromBits ε)
  print (fromBits o₂ * fromBits co₂)
