module Day11 where

import Data.Map (Map)
import qualified Data.Map as M

import AOC
import Intcode

type Hull = Map Coords Integer

getColour :: Coords -> Hull -> Integer
getColour = M.findWithDefault 0

runRobot :: [Integer] -> Integer -> Hull
runRobot program c = go (M.singleton p0 c) p0 (0, -1) (runIntcode program) where
    p0 = (0, 0)
    go h p d (Halt _) = h
    go h p d (Input f) = case f (getColour p h) of
        Output c (Output w e) ->
            let h' = M.insert p c h
                d' = (if w == 0 then ccw else cw) d
                p' = add d' p
            in go h' p' d' e

extend (lx, ly, ux, uy) (x, y) = (min lx x, min ly y, max ux x, max uy y)

main = do
    program <- parseProgram <$> getContents
    print . length $ runRobot program 0
    let hull = runRobot program 1
        (lx, ly, ux, uy) = foldl extend (0, 0, 0, 0) (M.keys hull)
    putStr $ unlines [[" █" !! fromIntegral (getColour (x, y) hull) | x <- [lx..ux]] | y <- [ly..uy]]
