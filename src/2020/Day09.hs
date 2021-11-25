module Day09 where

import           Data.Set (Set)
import qualified Data.Set as Set

import AOC

noSum ns = last ns `Set.notMember` sums
    where
        ts = filter (\x -> length x > 0) $ tails $ init ns
        sums = foldr (\(x:xs) ss -> Set.fromList (map (+x) xs) <> ss) Set.empty ts

main = do
    nums <- parseInput $ eachLine decimal
    let blocks = filter (\x -> length x == 26) $ map (take 26) $ tails nums
        invalid = last $ head [t | t <- blocks, noSum t]
    print invalid
    let ts = filter (\x -> length x >= 2) $ tails nums
        segs = concatMap (\x -> filter (\y -> length y >= 2) $ inits x) ts
        x = head $ filter (\x -> sum x == invalid) segs
    print $ minimum x + maximum x
