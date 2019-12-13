{-# LANGUAGE RecursiveDo #-}

import Data.Map (Map)
import qualified Data.Map as M

import Intcode

numberOfBlocks :: [Integer] -> Int
numberOfBlocks = go M.empty where
    go s (x:y:b:xs) | x >= 0 = go s' xs where
        s' = M.insert (x, y) b s
    go s [] = length $ M.filter (== 2) s

moves :: [Integer] -> [Integer]
moves = go 0 where
    go p (-1:_:_:xs) = go p xs
    go _ (x:y:3:xs) = go x xs
    go p (x:y:4:xs) = d:go p xs where
        d | p < x = 1
          | p > x = -1
          | otherwise = 0
    go p (_:_:_:xs) = go p xs
    go _ [] = []

finalScore :: [Integer] -> Integer
finalScore = go 0 where
    go _ (-1:_:s':xs) = go s' xs
    go s (_:_:_:xs) = go s xs
    go s [] = s

main :: IO ()
main = mdo
    program <- parseProgram <$> readFile "input13"
    print . numberOfBlocks =<< runIntcode program []
    output <- runIntcode (2:tail program) (moves output)
    print $ finalScore output
