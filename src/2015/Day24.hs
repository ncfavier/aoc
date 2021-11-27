module Day24 where

import AOC

main :: IO ()
main = do
  weights <- parseInput (eachLine number)
  for_ [3, 4] \nGroups -> do
    let goal = sum weights `div` nGroups
        solve 0 _ = pure []
        solve _ [] = empty
        solve goal (w:ws) | w <= goal = (w:) <$> solve (goal - w) ws <|> solve goal ws
                          | otherwise = solve goal ws
    print $ product $ head $ sortOn (length &&& product) (solve goal weights :: [[Int]])
