module Day08 where

import AOC

import Data.Map qualified as M
import Data.Set qualified as S

format = eachLine do
  digits <- word `endBy` " "
  "| "
  outputs <- word `sepBy` " "
  pure (map S.fromList digits, map S.fromList outputs)

decode (digits, outputs) = r
  where
    [one] = [d | d <- digits, length d == 2]
    [eight] = [d | d <- digits, length d == 7]
    [seven] = [d | d <- digits, length d == 3]
    [four] = [d | d <- digits, length d == 4]
    [three] = [d | d <- digits, length d == 5, seven `S.isSubsetOf` d]
    l = four S.\\ one
    [five] = [d | d <- digits, length d == 5, l `S.isSubsetOf` d]
    [two] = [d | d <- digits, length d == 5, d /= five, d /= three]
    [zero] = [d | d <- digits, length d == 6, not (l `S.isSubsetOf` d)]
    [nine] = [d | d <- digits, length d == 6, one `S.isSubsetOf` d, d /= zero]
    [six] = [d | d <- digits, length d == 6, d /= nine, d /= zero]
    m = M.fromList [(zero,0),(one,1),(two,2),(three,3),(four,4),(five,5),(six,6),(seven,7),(eight,8),(nine,9)]
    [a,b,c,d] = map (m M.!) outputs
    r = a*1000+b*100+c*10+d

main :: IO ()
main = do
  input <- parseInput format
  print $ howMany ((`elem` [2,3,4,7]) . length) $ concatMap snd input
  print $ sum $ decode <$> input
