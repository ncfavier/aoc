module Day23 where

import Control.Monad.ST
import Data.Array.ST

import AOC

complete :: [a] -> [a] -> [a]
complete (x:xs) (y:ys) = x:complete xs ys
complete    xs     []  = []
complete    []     ys  = ys

crabulate :: Int -> Int -> [Int] -> [Int]
crabulate size moves cups = runST do
    ring <- newArray_ (1, size) :: ST s (STUArray s Int Int)
    let right = readArray ring
        setRight = writeArray ring
        ringList = complete cups [1..size]
    zipWithM_ setRight ringList (tail $ cycle ringList)
    let cupsOrder 0 _ = pure []
        cupsOrder n cup = do
            next <- right cup
            (cup:) <$> cupsOrder (n - 1) next
        loop 0 _ = pure ()
        loop n cur = do
            moved@[front, _, back] <- cupsOrder 3 =<< right cur
            let dest = head $ filter (`notElem` moved) $ [cur-1,cur-2..1] ++ [size,size-1..cur+1]
            cur' <- right back
            setRight cur cur'
            setRight back =<< right dest
            setRight dest front
            loop (n - 1) cur'
    loop moves (head cups)
    cupsOrder size 1

main :: IO ()
main = do
    cups <- map digitToInt . head . lines <$> readInput
    putStrLn $ map intToDigit $ tail $ crabulate 9 100 cups
    let _1:x:y:_ = crabulate 1000000 10000000 cups
    print (x * y)
