{-# LANGUAGE RecursiveDo #-}

import Prelude hiding (Either(..))
import Control.Arrow
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
getColour p h = maybe 0 id $ M.lookup p h

runRobot :: Integer -> [Integer] -> IO Hull
runRobot c program = mdo
    hull <- newIORef (M.singleton (0, 0) c)
    let draw p d [] = return []
        draw p d (c:w:xs) = do
            h <- readIORef hull
            let h' = M.insert p c h
                d' = turn d w
                p' = forward d' p
                c' = getColour p' h'
            writeIORef hull h'
            (c':) <$> draw p' d' xs
    output <- runIntcode program (c:input)
    input <- draw (0, 0) Up output
    readIORef hull

extend (lx, ly, ux, uy) (x, y) = (min lx x, min ly y, max ux x, max uy y)

main = do
    program <- parseProgram <$> readFile "input11"
    print . length =<< runRobot 0 program
    hull <- runRobot 1 program
    let (lx, ly, ux, uy) = foldl extend (0, 0, 0, 0) (M.keys hull)
    putStr $ unlines [[" █" !! fromIntegral (getColour (x, y) hull) | x <- [lx..ux]] | y <- [ly..uy]]