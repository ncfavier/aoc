module Day08 where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Lens.Micro.Platform

import AOC

data Op = Acc | Nop | Jmp
        deriving (Eq, Ord)

type Instruction = (Op, Int)

instruction :: Parser Instruction
instruction = (,) <$> op <* " " <*> number
    where op = Acc <$ "acc" <|> Nop <$ "nop" <|> Jmp <$ "jmp"

main = do
    program <- parseInput $ eachLine instruction
    let run ins n acc seen = do
            -- if n `Set.member` seen then acc else
                if n >= length ins then [(True, n, acc)] else
                    let seen' = Set.insert n seen in
                    (False, n, acc):case ins !! n of
                        (Acc, i) -> run ins (n+1) (acc+i) seen'
                        (Nop, _) -> run ins (n+1) acc seen'
                        (Jmp, i) -> run ins (n+i) acc seen'
    let alteredPrograms = do
            i <- [0..length program - 1]
            guard (fst (program !! i) `elem` [Nop, Jmp])
            pure $ program & ix i . _1 %~ \case Nop -> Jmp; Jmp -> Nop
    print $ head $ filter (\(a, b, c) -> a) $ concat $ transpose [run ins 0 0 Set.empty | ins <- alteredPrograms]
