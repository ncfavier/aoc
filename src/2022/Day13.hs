module Day13 where

import AOC hiding (List)

data Value = Int Int | List [Value] deriving Eq

instance Ord Value where
  compare (Int a) (Int b) = compare a b
  compare (List a) (List b) = compare a b
  compare (List a) b = compare (List a) (List [b])
  compare a (List b) = compare (List [a]) (List b)

format = pair `sepBy` newline where
  pair = (,) <$> value <* newline <*> value <* newline
  value = Int <$> decimal <|> List <$> between "[" "]" (value `sepBy` ",")

main :: IO ()
main = do
  pairs <- parseInput format
  print $ sum [i | (i, (a, b)) <- zip [1..] pairs, a < b]
  let decoders = [List [List [Int 2]], List [List [Int 6]]]
  print $ product [i | (i, v) <- zip [1..] (sort (decoders ++ pairs ^.. each.each)), v `elem` decoders]
