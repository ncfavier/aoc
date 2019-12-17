{-# LANGUAGE RecursiveDo #-}
module Day11 where

import Prelude hiding (Either(..))
import Control.Arrow
import Control.Exception
import qualified Data.Map as M
import Data.IORef

import Intcode

data Direction = Up | Right | Down | Left
               deriving Enum

type Position = (Integer, Integer)

type Hull = M.Map Position Integer

turn :: Direction -> Integer -> Direction
turn d w = toEnum ((if w == 0 then pred else succ) (fromEnum d) `mod` 4)

forward :: Direction -> Position -> Position
forward Up    = (id *** pred)
forward Down  = (id *** succ)
forward Left  = (pred *** id)
forward Right = (succ *** id)

getColour :: Position -> Hull -> Integer
getColour = M.findWithDefault 0

runRobot :: [Integer] -> Integer -> IO Hull
runRobot program c = mdo
    hull <- newIORef (M.singleton (0, 0) c)
    let draw p d [] = return []
        draw p d (c:w:xs) = do
            h <- readIORef hull
            let h' = M.insert p c h
                d' = turn d w
                p' = forward d' p
                c' = getColour p' h'
            writeIORef hull h'
            c' <: draw p' d' xs
    output <- runIntcode program (c:input)
    input <- draw (0, 0) Up output
    evaluate (length output)
    readIORef hull

extend (lx, ly, ux, uy) (x, y) = (min lx x, min ly y, max ux x, max uy y)

main = do
    program <- parseProgram <$> getContents
    print . length =<< runRobot program 0
    hull <- runRobot program 1
    let (lx, ly, ux, uy) = foldl extend (0, 0, 0, 0) (M.keys hull)
    putStr $ unlines [[" █" !! fromIntegral (getColour (x, y) hull) | x <- [lx..ux]] | y <- [ly..uy]]
