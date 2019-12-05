{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
import Control.Monad
import Control.Monad.State
import Data.Array.MArray
import Data.Array.IO
import Data.List
import Data.IORef
import Data.Tuple
import Data.Bool

split :: Char -> String -> [String]
split d = words . map (\c -> if c == d then ' ' else c)

io = liftIO
(.:) = (.) . (.)

run :: [Integer] -> [Integer] -> IO ()
run program inputs = do
    let n = genericLength program
    mem <- newListArray (0, pred n) program :: IO (IOArray Integer Integer)
    pc <- newIORef 0
    ins <- newIORef inputs
    outs <- newIORef []
    let readMem r    = io $ readArray mem r
        writeMem w a = io $ writeArray mem w a
        readPC       = io $ readIORef pc
        incPC        = io $ modifyIORef' pc (+1)
        jump j       = io $ writeIORef pc j
        getInput     = io $ head <$> readIORef ins <* modifyIORef' ins tail
        putOutput v  = io $ modifyIORef' outs (v:)
        next = readMem =<< readPC <* incPC
        nextMode = state $ swap . (`divMod` 10)
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
            let (modes, op) = ins `divMod` 100
            flip runStateT modes $ case op of
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
    print . head =<< readIORef outs

main :: IO ()
main = do
    program <- map read . split ',' <$> readFile "input5"
    run program [1]
    run program [5]
