{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module Intcode (runIntcode) where

import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import Data.Array.MArray
import Data.Array.ST
import Data.List
import Data.STRef
import Data.Tuple
import Data.Bool

(.:) = (.) . (.)

runIntcode :: [Integer] -> [Integer] -> [Integer]
runIntcode program inputs = runST $ do
    let n = genericLength program
    mem <- newListArray (0, pred n) program :: ST s (STArray s Integer Integer)
    pc <- newSTRef 0
    ins <- newSTRef inputs
    outs <- newSTRef []
    modes <- newSTRef 0
    let readMem r    = readArray mem r
        writeMem w a = writeArray mem w a
        readPC       = readSTRef pc
        incPC        = modifySTRef' pc (+1)
        jump j       = writeSTRef pc j
        getInput     = head <$> readSTRef ins <* modifySTRef' ins tail
        putOutput v  = modifySTRef' outs (v:)
        next = readMem =<< readPC <* incPC
        nextMode = do
            (modes', mode) <- (`divMod` 10) <$> readSTRef modes
            writeSTRef modes modes'
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
            writeSTRef modes ms
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
    readSTRef outs

-- runNounVerb :: [Integer] -> Integer -> Integer -> Integer
-- runNounVerb program noun verb = head . fst $ runIntcode program' []
--     where program' = head program:[noun, verb] ++ drop 3 program
