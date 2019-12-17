module Day2 where

import Control.Monad
import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import Data.List
import Data.STRef

split :: Char -> String -> [String]
split d = words . map (\c -> if c == d then ' ' else c)

run :: [Integer] -> Integer -> Integer -> Integer
run program noun verb = runST $ do
    let n = genericLength program
    mem <- newListArray (0, pred n) program :: ST s (STArray s Integer Integer)
    pc <- newSTRef 0
    let next = readArray mem =<< readSTRef pc <* modifySTRef' pc (+1)
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

main :: IO ()
main = do
    program <- map read . split ',' <$> getContents
    print $ run program 12 2
    print $ head [ 100 * noun + verb
                 | noun <- [0..99]
                 , verb <- [0..99]
                 , run program noun verb == 19690720
                 ]
