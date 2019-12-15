{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module Intcode (parseProgram, runIntcode, (<:)) where

import Control.Monad
import Data.Array.MArray
import Data.Array.IO
import Data.List
import Data.IORef
import Data.Tuple
import Data.Bool
import System.IO.Unsafe

data Operand = Absolute Integer | Immediate Integer | Relative Integer

(.:) :: (a -> b) -> (c -> d -> a) -> (c -> d -> b)
(.:) = (.) . (.)

(<:) :: a -> IO [a] -> IO [a]
x <: m = (x:) <$> unsafeInterleaveIO m

split :: Char -> String -> [String]
split d = words . map (\c -> if c == d then ' ' else c)

parseProgram :: String -> [Integer]
parseProgram = map read . split ','

runIntcode :: [Integer] -> [Integer] -> IO [Integer]
runIntcode program inputs = do
    mem <- newListArray (0, 32767) (program ++ repeat 0) :: IO (IOArray Integer Integer)
    pc <- newIORef 0
    modes <- newIORef 0
    base <- newIORef 0
    let readMem r    = readArray mem r
        writeMem w a = writeArray mem w a
        readPC       = readIORef pc
        incPC        = modifyIORef' pc (+ 1)
        jump j       = writeIORef pc j
        moveBase o   = modifyIORef' base (+ o)
        next = readMem =<< readPC <* incPC
        nextMode = do
            (modes', mode) <- (`divMod` 10) <$> readIORef modes
            writeIORef modes modes'
            return mode
        operand = do
            n <- next
            mode <- nextMode
            return $ case mode of
                0 -> Absolute n
                1 -> Immediate n
                2 -> Relative n
        readOperand = do
            o <- operand
            case o of
                Absolute n -> readMem n
                Immediate n -> return n
                Relative n -> readMem . (+ n) =<< readIORef base
        writeOperand = do
            o <- operand
            case o of
                Absolute n -> return n
                Relative n -> (+ n) <$> readIORef base
        binary (?) = do
            a <- readOperand
            b <- readOperand
            r <- writeOperand
            writeMem r (a ? b)
        test (?) = binary (bool 0 1 .: (?))
        conditionalJump p = do
            c <- readOperand
            j <- readOperand
            when (p c) (jump j)
        loop inputs = do
            ins <- next
            let (ms, op) = ins `divMod` 100
            writeIORef modes ms
            case op of
                1 -> binary (+) >> loop inputs
                2 -> binary (*) >> loop inputs
                3 -> do
                    a <- writeOperand
                    case inputs of
                        i:is -> do
                            writeMem a i
                            loop is
                        _ -> return []
                4 -> do
                    a <- readOperand
                    a <: loop inputs
                5 -> conditionalJump (/= 0) >> loop inputs
                6 -> conditionalJump (== 0) >> loop inputs
                7 -> test (<) >> loop inputs
                8 -> test (==) >> loop inputs
                9 -> do
                    a <- readOperand
                    moveBase a
                    loop inputs
                99 -> return []
                _ -> fail $ "unknown opcode " ++ show op
    loop inputs
