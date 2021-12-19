module Day19 where

import AOC hiding (match)

import Data.Map qualified as M
import Data.Set qualified as S

type C = V3 Int

format :: Parser (Map Int (Set C))
format = M.fromList . zip [0..] <$> scanner `sepBy` newline
  where
    beacon = (\[a,b,c] -> V3 a b c) <$> number `sepBy1` ","
    scanner = do
      "--- scanner " <* decimal <* " ---" <* newline
      S.fromList <$> beacon `endBy` newline

reorient :: C -> [C]
reorient (V3 a b c) =
  [ V3 ap bp cp * V3 as bs cs
  | ([ap, bp, cp], sign) <- zip (permutations [a, b, c]) [1, -1, -1, 1, 1, -1]
  , [as, bs] <- replicateM 2 [1, -1]
  , let cs = as * bs * sign
  ]

match :: Int -> Set C -> Set C -> [(C, Set C)]
match overlap relative fixed =
  [ (t, (+ t) `S.mapMonotonic` relative)
  | (t, n) <- M.assocs translations
  , n >= overlap
  ] where
    translations = counts $ (-) <$> S.toList fixed <*> S.toList relative

assemble :: Map Int (Set C) -> ([C], Set C)
assemble scans = (scanners, fold beacons)
  where
    (scanners, beacons) = unzip $ M.elems $ fixedPointOn M.keys step start
    start = M.singleton 0 (0, scans M.! 0)
    step known = known <> M.fromList
      [ (i, absolute)
      | (i, scan) <- M.assocs (scans M.\\ known)
      , relative <- transposeOf traverseSet (reorient `S.mapMonotonic` scan)
      , (_, fixed) <- M.elems known
      , absolute:_ <- [match 12 relative fixed]
      ]

main :: IO ()
main = do
  (scanners, beacons) <- assemble <$> parseInput format
  print $ length beacons
  print $ maximum [sum (abs (a - b)) | (a, b) <- pairs scanners]

