{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
module Intcode where

import Control.Monad.State
import Data.Char
import Data.Bool
import Data.List.Split
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Map (Map)
import qualified Data.Map as M

import AOC hiding (set)

data Memory = Memory { readOnly :: Vector Integer
                     , overlay  :: Map Integer Integer
                     }

data Machine = Machine { mem   :: Memory
                       , pc    :: Integer
                       , base  :: Integer
                       , modes :: Integer
                       }

data Effect = Input (Integer -> Effect)
            | Output Integer Effect
            | Halt Memory

newMemory program = Memory (V.fromList program) M.empty

infixl 8 !!!
Memory{..} !!! i | Just v <- M.lookup i overlay = v
                 | Just v <- readOnly V.!? fromIntegral i = v
                 | otherwise = 0
set i v mem = mem { overlay = M.insert i v (overlay mem) }

newMachine program = Machine (newMemory program) 0 0 0

parseProgram :: String -> [Integer]
parseProgram = map read . splitOn ","

parseInputProgram :: IO [Integer ]
parseInputProgram = parseProgram <$> readInput

incPc = state \m -> (pc m, m { pc = succ (pc m) })
nextMode = state \m -> let (modes', mode) = modes m `divMod` 10
                       in (mode, m { modes = modes' })

nextOp = do
    pc <- incPc
    mem <- gets mem
    return $ mem !!! pc

nextArg = do
    pc <- incPc
    mode <- nextMode
    base <- gets base
    mem <- gets mem
    return case mode of
        0 -> mem !!! pc
        1 -> pc
        2 -> base + mem !!! pc

readArg = do
    arg <- nextArg
    mem <- gets mem
    return (mem !!! arg)

write v = do
    arg <- nextArg
    modify \m -> m { mem = set arg v (mem m) }

bin (?) = do
    a <- readArg
    b <- readArg
    write (a ? b)

conditionalJump p = do
    v <- readArg
    a <- readArg
    when (p v) (jump a)

jump pc = modify \m -> m { pc }

test (?) = bin \a b -> bool 0 1 (a ? b)

moveBase o = modify \m -> m { base = base m + o }

runIntcode :: [Integer] -> Effect
runIntcode program = evalState loop (newMachine program) where
    loop = do
        op' <- nextOp
        let (modes, op) = op' `divMod` 100
        modify \m -> m { modes }
        case op of
            1 -> bin (+) >> loop
            2 -> bin (*) >> loop
            3 -> do
                m <- get
                return $ Input \v -> evalState (write v >> loop) m
            4 -> do
                v <- readArg
                m <- get
                return $ Output v $ evalState loop m
            5 -> conditionalJump (/= 0) >> loop
            6 -> conditionalJump (== 0) >> loop
            7 -> test (<) >> loop
            8 -> test (==) >> loop
            9 -> readArg >>= moveBase >> loop
            99 -> do
                mem <- gets mem
                return $ Halt mem

feed :: [Integer] -> Effect -> Effect
feed [] e = e
feed (x:xs) (Input f) = feed xs (f x)
feed xs (Output i e) = Output i (feed xs e)
feed _ e = e

intcodeToList :: [Integer] -> [Integer] -> [Integer]
intcodeToList program = go (runIntcode program) where
    go (Halt _) _ = []
    go (Output v e) inp = v:go e inp
    go (Input f) (i:is) = go (f i) is
    go (Input _) _ = error "no input"

asciiToIntegers :: String -> [Integer]
asciiToIntegers = map (fromIntegral . ord)

integersToAscii :: [Integer] -> String
integersToAscii = map (chr . fromIntegral)
