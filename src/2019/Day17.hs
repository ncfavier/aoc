module Day17 where

import Data.Map (Map)
import qualified Data.Map as M

import AOC
import Intcode

directions :: [Coords]
directions = [(-1, 0), (0, -1), (1, 0), (0, 1)]

neighbours :: Coords -> [Coords]
neighbours p = map (p +) directions

groupMoves :: String -> [String]
groupMoves p = go (group p) where
    go ([d]:f@('F':_):p) = (d:',':show (length f)):go p
    go [] = []

maxLength, nFunctions :: Int
maxLength = 20
nFunctions = 3

shortEnough :: String -> Bool
shortEnough p = length p <= maxLength

compress :: [String] -> [(String, [String])]
compress = go [] [] where
    go fs acc [] = [ (main, [intercalate "," ins | (_, ins) <- reverse fs])
                   | let main = intercalate "," (reverse acc)
                   , shortEnough main ]
    go fs acc p = do (f, ins) <- fs
                     Just rest <- return $ stripPrefix ins p
                     go fs (f:acc) rest
              <|> do guard $ length fs < nFunctions
                     (ins, rest) <- takeWhile (shortEnough . intercalate "," . fst) $ tail $ zip (inits p) (tails p)
                     let f = [chr (ord 'A' + length fs)]
                     go ((f, ins):fs) (f:acc) rest

main :: IO ()
main = do
    program <- parseInputProgram
    let output = integersToAscii $ intcodeToList program []
        grid   = M.fromList [((x, y), c) | (y, row) <- zip [0..] (lines output), (x, c) <- zip [0..] row]
        tile p = M.findWithDefault '.' p grid
    print $ sum [ x * y
                | (p@(x, y), '#') <- M.toList grid
                , all (\p -> tile p == '#') (neighbours p)
                ]
    let path p d | canGo d       = 'F':path (p + d) d
                 | canGo (ccw d) = 'L':path p           (ccw d)
                 | canGo (cw d)  = 'R':path p           (cw d)
                 | otherwise     = []
                 where canGo d = tile (p + d) == '#'
        (start, d)   = head [ (p, directions !! d)
                            | (p, c) <- M.toList grid
                            , Just d <- [c `elemIndex` "<^>v"] ]
        (main, fs):_ = compress $ groupMoves $ path start d
    print . last . intcodeToList (2:tail program) . asciiToIntegers $ unlines (main:fs ++ ["n"])
