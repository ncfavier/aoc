module Day01 where

import AOC

main :: IO ()
main = do
  input <- parseInput (eachLine takeRest)
  print $ sum $ map (read . sequence [head, last] . filter isDigit) $ input
  let strings = ([(show i, head (show i)) | i <- [1..9]] ++ [("one", '1'), ("two", '2'), ("three", '3'), ("four", '4'), ("five", '5'), ("six", '6'), ("seven", '7'), ("eight", '8'), ("nine", '9')])
      search strings s = [b | (a, b) <- strings, a `isPrefixOf` s] <> search strings (tail s)
      number = read . sequence [head . search strings, head . search (map (first reverse) strings) . reverse]
  print $ sum $ map number input
