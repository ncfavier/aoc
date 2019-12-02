{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad
import Data.List
import Data.Array.MArray
import Data.Array.IO
import Data.IORef
import System.Exit

split d = words . map (\c -> if c == d then ' ' else c)

runProgram :: [Integer] -> Integer -> Integer -> IO Integer
runProgram program noun verb = do
    let n = genericLength program
    mem :: IOArray Integer Integer <- newListArray (0, n - 1) program
    pc <- newIORef 0
    let next = (readArray mem =<< readIORef pc) <* modifyIORef' pc (+1)
        binop (.) = do
            a <- readArray mem =<< next
            b <- readArray mem =<< next
            r <- next
            writeArray mem r (a . b)
        loop = do
            op <- next
            continue <- case op of
                1 -> True <$ binop (+)
                2 -> True <$ binop (*)
                99 -> return False
            when continue loop
    writeArray mem 1 noun
    writeArray mem 2 verb
    loop
    readArray mem 0

main = do
    program <- map read . split ',' <$> readFile "input2"
    print =<< runProgram program 12 2
    let try noun verb = do
            r <- runProgram program noun verb
            when (r == 19690720) $ do
                print (100 * noun + verb)
                exitSuccess
    sequence_ [try noun verb | noun <- [0..99], verb <- [0..99]]
