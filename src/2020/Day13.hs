module Day13 where

import Math.NumberTheory.Moduli.Class
import Math.NumberTheory.Moduli.Chinese

import AOC

schedule :: Parser (Integer, [(Integer, Integer)])
schedule = do
    departure <- lexeme number
    buses <- (Just <$> lexeme number <|> Nothing <$ "x") `sepBy` ","
    pure (departure, [(i, m) | (i, Just m) <- zip [0..] buses])

main = do
    (departure, buses) <- parseInput schedule
    print $ uncurry (*) $ minimum [((-departure) `mod` m, m) | (_, m) <- buses]
    let residues = [(-i) `modulo` fromIntegral m | (i, m) <- buses]
    case foldrM chineseSomeMod (0 `modulo` 1) residues of
        Just (SomeMod n) -> print (getVal n)
