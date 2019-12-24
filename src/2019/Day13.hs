{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Day13 where

import Data.Map (Map)
import qualified Data.Map as M

import Intcode

data GameState = GameState { score   :: Integer
                           , paddleX :: Integer
                           , ballX   :: Integer
                           }

numberOfBlocks :: [Integer] -> Int
numberOfBlocks program = go M.empty (intcodeToList program []) where
    go s (x:y:b:xs) | x >= 0 = go s' xs
        where s' = M.insert (x, y) b s
    go s [] = length $ M.filter (== 2) s

finalScore :: [Integer] -> Integer
finalScore program = go (GameState 0 0 0) (runIntcode program) where
    go s (Halt _) = score s
    go s (Input f) = go s (f m)
        where m | paddleX s < ballX s = 1
                | paddleX s > ballX s = -1
                | otherwise           = 0
    go s (Output (-1) (Output _ (Output score e))) = go s { score } e
    go s (Output paddleX (Output y (Output 3 e))) = go s { paddleX } e
    go s (Output ballX (Output y (Output 4 e))) = go s { ballX } e
    go s (Output _ (Output _ (Output _ e))) = go s e

main :: IO ()
main = do
    program <- parseProgram <$> getContents
    print $ numberOfBlocks program
    print $ finalScore (2:tail program)
