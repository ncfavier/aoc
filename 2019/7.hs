{-# LANGUAGE RecursiveDo #-}
import Data.List

import Intcode

split :: Char -> String -> [String]
split d = words . map (\c -> if c == d then ' ' else c)

run :: [Integer] -> Integer -> Integer -> Integer
run program signal setting = head $ runIntcode program [setting, signal]

main :: IO ()
main = do
    program <- map read . split ',' <$> readFile "test7" --"input7"
    -- print $ maximum [foldl' (run program) 0 settings | settings <- permutations [0..4]]
    runLoop program [9,8,7,6,5]

runLoop program [a,b,c,d,e] = mdo
    outA <- return $ runIntcode program (a:outE)
    outB <- return $ runIntcode program (b:outA)
    outC <- return $ runIntcode program (c:outB)
    outD <- return $ runIntcode program (d:outC)
    outE <- return $ runIntcode program (e:outD)
    print $ outE
