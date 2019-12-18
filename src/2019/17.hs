module Day17 where

import Control.Monad
import Control.Applicative
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as M

import Intcode

type Coords = (Integer, Integer)

add :: Coords -> Coords -> Coords
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

directions :: [Coords]
directions = [(-1, 0), (0, -1), (1, 0), (0, 1)]

neighbours :: Coords -> [Coords]
neighbours p = map (add p) directions

left, right :: Coords -> Coords
left  (x, y) = (y, -x)
right (x, y) = (-y, x)

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
    program <- parseProgram <$> getContents
    output  <- map (chr . fromIntegral) <$> runIntcode program []
    let grid   = M.fromList [((x, y), c) | (y, row) <- zip [0..] (lines output), (x, c) <- zip [0..] row]
        tile p = M.findWithDefault '.' p grid
    print $ sum [ x * y
                | (p@(x, y), '#') <- M.toList grid
                , all (\p -> tile p == '#') (neighbours p)
                ]
    let path p d | canGo d         = 'F':path (p `add` d) d
                 | canGo (left d)  = 'L':path p           (left d)
                 | canGo (right d) = 'R':path p           (right d)
                 | otherwise       = []
                 where canGo d = tile (p `add` d) == '#'
        (start, d)   = head [ (p, directions !! d)
                            | (p, c) <- M.toList grid
                            , Just d <- [c `elemIndex` "<^>v"] ]
        (main, fs):_ = compress $ groupMoves $ path start d
    output' <- runIntcode (2:tail program) $ map (toInteger . ord) $ unlines (main:fs ++ ["n"])
    print (last output')
