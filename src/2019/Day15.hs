{-# LANGUAGE RecordWildCards #-}
module Day15 where

import Data.Map (Map)
import qualified Data.Map as M

import AOC
import Intcode

move :: Integer -> Coords -> Coords
move 1 = (id *** pred)
move 2 = (id *** succ)
move 3 = (pred *** id)
move 4 = (succ *** id)

data DroidState = DroidState { position :: Coords
                             , space    :: Map Coords Integer
                             }

initialState = DroidState (0, 0) (M.singleton (0, 0) 1)

shortestPath :: Coords -> (Coords -> Bool) -> Map Coords Integer -> Either Integer [Integer]
shortestPath s t space = go (M.singleton s []) [s] 0 where
    go _ [] depth = Left (depth - 1)
    go seen ps depth
        | p:_ <- filter t ps = Right $ reverse (seen M.! p)
        | otherwise =
            let ns = [ (n, d:seen M.! p)
                     | p <- ps
                     , d <- [1..4]
                     , let n = move d p
                     , n `M.notMember` seen
                     , (n `M.member` space && space M.! n /= 0) || t n
                     ]
            in go (M.union seen (M.fromList ns)) [p | (p, _) <- ns] (depth + 1)

runDroid :: [Integer] -> (Int, Integer)
runDroid program = explore initialState (runIntcode program) where
    explore s@DroidState {..} e = do
        case shortestPath position (`M.notMember` space) space of
            Right path -> go path s e
            Left _ ->
                let (oxygen, _) = M.findMin (M.filter (== 2) space)
                    Right pathToOxygen = shortestPath (0, 0) (== oxygen) space
                    Left maxDepth = shortestPath oxygen (const False) space
                in (length pathToOxygen, maxDepth)
    go [] s e = explore s e
    go (p:ps) DroidState {..} (Input f) = go ps (DroidState position' space') e
        where Output o e = f p
              target = move p position
              position' | o == 0    = position
                        | otherwise = target
              space' = M.insert target o space

main :: IO ()
main = do
    program <- parseProgram <$> readInput
    let (distanceToOxygen, maxDepth) = runDroid program
    print distanceToOxygen
    print maxDepth
