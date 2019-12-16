{-# LANGUAGE RecursiveDo #-}
module Day7 where

import Data.List

import Intcode

runLoop :: [Integer] -> [Integer] -> IO Integer
runLoop program [a, b, c, d, e] = mdo
    outA <- runIntcode program (a:0:outE)
    outB <- runIntcode program (b:outA)
    outC <- runIntcode program (c:outB)
    outD <- runIntcode program (d:outC)
    outE <- runIntcode program (e:outD)
    return $ last outE

main :: IO ()
main = do
    program <- parseProgram <$> getContents
    print . maximum =<< mapM (runLoop program) (permutations [0..4])
    print . maximum =<< mapM (runLoop program) (permutations [5..9])
