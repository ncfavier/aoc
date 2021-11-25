module Day10 where

import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.IntSet as IS

import AOC

main = do
    input <- parseInput $ eachLine decimal
    let nums = 0:sort input ++ [device]
        device = maximum input + 3
        diffs = counts (zipWith subtract nums (tail nums))
    print (diffs Map.! 1 * diffs Map.! 3)
    let nums' = IS.fromList nums
        arrangements = fixMem [0..device] \arrangements start -> if
            | start == device -> 1
            | otherwise -> sum
                [ arrangements (start + i)
                | i <- [1..3]
                , (start + i) `IS.member` nums' ]
    print (arrangements 0)
