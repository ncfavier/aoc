module Day11 where

import           Data.Map (Map)
import qualified Data.Map as Map

import AOC hiding (evolve)

evolve :: Integer
       -> (Map Coords Char -> Coords -> Coords -> Maybe Char)
       -> Map Coords Char
       -> Map Coords Char
evolve crowded neighbour grid = Map.mapWithKey f grid where
    f p t | t == 'L' && n == 0       = '#'
          | t == '#' && n >= crowded = 'L'
          | otherwise                = t
          where neighbours = mapMaybe (neighbour grid p) principal
                n = howMany (== '#') neighbours

main :: IO ()
main = do
    (Map.filter (/= '.') -> grid, width, height) <- makeGrid <$> readInput
    let direct grid p d = grid Map.!? (p + d)
        visible grid p d = asum (map (grid Map.!?) sight)
            where sight = takeWhile (inRange ((0, 0), (width, height)))
                        $ iterate1 (+ d) p
    for_ [(4, direct), (5, visible)] \(crowded, neighbour) ->
        print $ howMany (== '#')
              $ firstDuplicate
              $ iterate (evolve crowded neighbour) grid
