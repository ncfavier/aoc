{-# LANGUAGE OverloadedStrings #-}
module Day14 where

import Data.Map (Map)
import qualified Data.Map as M

import AOC

ingredient :: Parser (Integer, String)
ingredient = (,) <$> decimal <* " " <*> many letterChar

recipe :: Parser ([(Integer, String)], (Integer, String))
recipe = (,) <$> (ingredient `sepBy` ", ") <* " => " <*> ingredient

main :: IO ()
main = do
    rs <- parseInput $ eachLine recipe
    let recipes = M.fromList [(r, M.insert r (-n) $ M.fromList [(i, n) | (n, i) <- is]) | (is, (n, r)) <- rs]
        reduce g | Just (r, n) <- r' = let recipe = recipes M.! r
                                           f = n `div` (recipe M.! r)
                                       in reduce $ M.filter (/= 0) $ M.unionWith (+) g $ M.map (* (-f)) recipe
                 | otherwise         = g
                 where r' = M.lookupMin $ M.filterWithKey (\r n -> r /= "ORE" && n > 0) g
    let oreNeeded n = reduce (M.singleton "FUEL" n) M.! "ORE"
        oreForOneFuel = oreNeeded 1
    print oreForOneFuel
    let hold = 1000000000000
        search low up | up - low <= 1        = low
                      | oreNeeded mid > hold = search low mid
                      | otherwise            = search mid up
                      where mid = (low + up) `div` 2
        low = hold `div` oreForOneFuel
        up = until ((> hold) . oreNeeded) (2 *) low
    print (search low up)
