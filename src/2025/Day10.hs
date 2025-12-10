module Day10 where

import AOC

format = eachLine do
  lights <- hlexeme (between "[" "]" (many ((== '#') <$> oneOf ".#")))
  let mkList b = [i `elem` b | (i, _) <- zip [0..] lights]
  buttons <- many (mkList <$> hlexeme (between "(" ")" (decimal `sepBy` ",")))
  joltage <- hlexeme (between "{" "}" (decimal `sepBy` ","))
  pure (lights, buttons, joltage)

p1 (lights, buttons, _) = fromJust $ lookup lights $ bfs step [False <$ lights]
  where step l = zipWith xor l <$> buttons

-- too slow for the actual input
p2 (_, buttons, joltage) = fromJust $ lookup joltage $ astar step [0 <$ joltage]
  where
    increase j b = zipWith (+) j (bool 0 1 <$> b)
    heuristic j'
      | any (< 0) diffs = Nothing
      | otherwise = Just (maximum diffs)
      where diffs = zipWith (-) joltage j'
    step j = [(j', 1, g) | b <- buttons, let j' = increase j b, Just g <- [heuristic j']]

main = do
  input <- parseInput format
  print $ sum $ p1 <$> input
  print $ sum $ p2 <$> input
