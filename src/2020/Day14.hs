module Day14 where

import           Data.Bits
import           Data.Map (Map)
import qualified Data.Map as Map

import AOC

data Instruction = Mask String | Mem Integer Integer

mask, mem :: Parser Instruction
mask = Mask <$ "mask = " <*> many alphaNumChar
mem  = Mem <$ "mem[" <*> number <* "] = " <*> number

alterValue :: Integer -> [(Int, Char)] -> Integer
alterValue = foldl' \v -> \case
    (_, 'X') -> v
    (i, '1') -> v `setBit` i
    (i, '0') -> v `clearBit` i

alterAddress :: Integer -> [(Int, Char)] -> [Integer]
alterAddress = foldlM \v -> \case
    (i, 'X') -> [v `setBit` i, v `clearBit` i]
    (i, '1') -> [v `setBit` i]
    (_, '0') -> [v]

run :: Int -> [Instruction] -> Integer
run version = go (replicate 36 'X') Map.empty where
    go _    mem [] = sum mem
    go _    mem (Mask mask:ins) = go mask mem ins
    go mask mem (Mem a v:ins) = go mask mem' ins where
        mem' = case version of
            1 -> Map.insert a v' mem
            2 -> foldl' (\m a -> Map.insert a v m) mem as
        bits = zip [0..] (reverse mask)
        v' = alterValue v bits
        as = alterAddress a bits

main :: IO ()
main = do
    ins <- parseInputLines (mask <|> mem)
    for_ [1, 2] \version -> print (run version ins)
