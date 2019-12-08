{-# LANGUAGE RecursiveDo #-}
import Data.List

import Intcode

split :: Char -> String -> [String]
split d = words . map (\c -> if c == d then ' ' else c)

-- run :: [Integer] -> Integer -> Integer -> Integer
-- run program signal setting = head $ runIntcode program [setting, signal]

main :: IO ()
main = do
    program <- map read . split ',' <$> readFile "input7"
    -- print $ maximum [foldl' (run program) 0 settings | settings <- permutations [0..4]]
    runLoop program [9,8,7,6,5]

runLoop program [a,b,c,d,e] = mdo
    outA <- runIntcode program (a:0:outE)
    outB <- runIntcode program (b:outA)
    outC <- runIntcode program (c:outB)
    outD <- runIntcode program (d:outC)
    outE <- runIntcode program (e:outD)
    print outE
