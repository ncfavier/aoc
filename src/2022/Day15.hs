module Day15 where

import AOC

import Data.Interval as I
import Data.IntervalSet qualified as IS

format = eachLine sensor where
  sensor = (,) <$ "Sensor at " <*> position <* ": closest beacon is at " <*> position
  position = (,) <$ "x=" <*> number <* ", y=" <*> number

size = 4_000_000

main :: IO ()
main = do
  (sensors, beacons) <- unzip . map (\(s, b) -> ((s, manhattan (b - s)), b)) <$> parseInput format
  let rowCoverage y = IS.fromList (map coverage sensors) where
        coverage ((sx, sy), d) = Finite (sx - r) <=..<= Finite (sx + r) where r = d - abs (sy - y)
      noBeacon y = intervalSetCardinality $ rowCoverage y - IS.fromList [I.singleton bx | (bx, by) <- beacons, by == y]
  print (noBeacon (size `div` 2))
  let (x, y) = head do
        ((sx, sy), d) <- sensors
        i <- [0..d]
        let j = d + 1 - i
        b <- [(sx + j, sy + i), (sx - j, sy - i), (sx + i, sy + j), (sx - i, sy - j)]
        guard $ inRange ((0, 0), (size, size)) b
        guard $ all (\(s, d) -> manhattan (b - s) > d) sensors
        pure b
  print (x * size + y)
