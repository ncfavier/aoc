module Day06 where

import AOC

format = (,) <$ lexeme "Time:" <*> many (hlexeme number) <* newline <* lexeme "Distance:" <*> many (hlexeme number) <* newline

ways :: Integer -> Integer -> Integer
ways t d = b - a + 1
  where
    δ = sqrt $ fromIntegral (t * t - 4 * d)
    a = floor ((fromIntegral t - δ) / 2 + 1)
    b = ceiling ((fromIntegral t + δ) / 2 - 1)

confuse :: (Read a, Show a) => [a] -> a
confuse = read . concatMap show

main :: IO ()
main = do
  (times, distances) <- parseInput format
  print $ product $ zipWith ways times distances
  print $ ways (confuse times) (confuse distances)
