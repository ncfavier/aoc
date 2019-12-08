{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module Intcode (runIntcode) where

import Control.Monad
import Data.Array.MArray
import Data.Array.IO
import Data.List
import Data.IORef
import Data.Tuple
import Data.Bool

(.:) = (.) . (.)

runIntcode :: [Integer] -> [Integer] -> IO [Integer]
runIntcode program inputs = do
    let n = genericLength program
    mem <- newListArray (0, pred n) program :: IO (IOArray Integer Integer)
    pc <- newIORef 0
    ins <- newIORef inputs
    outs <- newIORef []
    modes <- newIORef 0
    let readMem r    = readArray mem r
        writeMem w a = writeArray mem w a
        readPC       = readIORef pc
        incPC        = modifyIORef' pc (+1)
        jump j       = writeIORef pc j
        getInput     = head <$> readIORef ins <* modifyIORef' ins tail
        putOutput v  = modifyIORef' outs (v:)
        next = readMem =<< readPC <* incPC
        nextMode = do
            (modes', mode) <- (`divMod` 10) <$> readIORef modes
            writeIORef modes modes'
            return mode
        operand = do
            n <- next
            mode <- nextMode
            (if mode == 0 then readMem else return) n
        writeOperand = next <* nextMode
        binary (?) = do
            a <- operand
            b <- operand
            r <- writeOperand
            writeMem r (a ? b)
        test (?) = binary (bool 0 1 .: (?))
        conditionalJump p = do
            c <- operand
            j <- operand
            when (p c) (jump j)
        loop = do
            ins <- next
            let (ms, op) = ins `divMod` 100
            writeIORef modes ms
            case op of
                1 -> binary (+)
                2 -> binary (*)
                3 -> do
                    a <- writeOperand
                    writeMem a =<< getInput
                4 -> putOutput =<< operand
                5 -> conditionalJump (/= 0)
                6 -> conditionalJump (== 0)
                7 -> test (<)
                8 -> test (==)
                99 -> return ()
            unless (op == 99) loop
    loop
    readIORef outs
