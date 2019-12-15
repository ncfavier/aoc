{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Arrow
import Control.Monad
import Control.Exception
import Data.Map (Map)
import qualified Data.Map as M

import Intcode

type Position = (Integer, Integer)

move :: Integer -> Position -> Position
move 1 = (id *** pred)
move 2 = (id *** succ)
move 3 = (pred *** id)
move 4 = (succ *** id)

data State = State { position :: Position
                   , space    :: Map Position Integer
                   , route    :: [Integer]
                   }

initialState = State (0, 0) (M.singleton (0, 0) 1) []

shortestPath :: Position -> (Position -> Bool) -> Map Position Integer -> Either Integer [Integer]
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

runDroid :: [Integer] -> IO [Integer]
runDroid = explore initialState where
    explore s@State {..} inputs = do
        case shortestPath position (`M.notMember` space) space of
            Right path -> head path <: listen s { route = path } inputs
            Left _ -> do
                let (oxygen, _) = M.findMin (M.filter (== 2) space)
                    Right pathToOxygen = shortestPath (0, 0) (== oxygen) space
                    Left maxDepth = shortestPath oxygen (const False) space
                print (length pathToOxygen)
                print maxDepth
                return []
    listen s@State {..} inputs = do
        let i:is = inputs
            r:rs = route
            target = move r position
            pos' = if i == 0 then position else target
            space' = M.insert target i space
            s' = s { position = pos', space = space', route = rs }
        case rs of
            r':_ -> r' <: listen s' is
            _    -> explore s' is

main = mdo
    program <- parseProgram <$> readFile "input15"
    input <- runDroid output
    output <- runIntcode program input
    evaluate (length output) -- force things to happen
