module Day16 where

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

import AOC

pattern :: Int -> [Int]
pattern i = tail $ cycle $ [0, 1, 0, -1] >>= replicate (i + 1)

ones :: Int -> Int
ones n = abs n `mod` 10

phase :: [Int] -> [Int]
phase l = [ones $ sum $ zipWith (*) l (pattern i) | i <- [0..pred (length l)]]

phase' :: Vector Int -> Vector Int
phase' = V.scanr1 f
    where a `f` b = ones (a + b)

main :: IO ()
main = do
    [input] <- lines <$> readInput
    let offset = read $ take 7 input
        l = map digitToInt input
    putStrLn $ concatMap show $ take 8 $ iterate phase l !! 100
    let v = V.fromList $ drop offset $ concat $ replicate 10000 l
    putStrLn $ concatMap show $ V.toList $ V.take 8 $ iterate phase' v !! 100
