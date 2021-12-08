module Day08 where

import AOC

import Data.Map qualified as M
import Data.Set qualified as S

format = eachLine do
  let pat = S.fromList <$> word
  (,) <$> pat `endBy` " " <* "| " <*> pat `sepBy` " "

decode (digits, outputs) = (ds M.!) <$> outputs
  where
    [d1, d7, d4, l51, l52, l53, l61, l62, l63, d8] = sortOn length digits
    [d2, d3, d5, d0, d6, d9] = sortOn
      (length &&& ((d4 S.\\ d1) `S.isSubsetOf`) &&& (d1 `S.isSubsetOf`))
      [l51, l52, l53, l61, l62, l63]
    ds = M.fromList [(d0,0),(d1,1),(d2,2),(d3,3),(d4,4),(d5,5),(d6,6),(d7,7),(d8,8),(d9,9)]

fromBase10 = foldl' (\a b -> a * 10 + b) 0

main :: IO ()
main = do
  displays <- parseInput format
  let outputs = decode <$> displays
  print $ howMany (`elem` [1, 4, 7, 8]) $ concat outputs
  print $ sum $ fromBase10 <$> outputs
