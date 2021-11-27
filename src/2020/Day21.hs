module Day21 where

import Data.Map qualified as Map
import Data.Set qualified as Set

import AOC

inputP :: Parser ([String], [String])
inputP = do
    ingredients <- word `endBy` " "
    "(contains "
    allergens <- word `sepBy` ", "
    ")"
    pure (ingredients, allergens)

solve :: [(String, Set String)] -> [[(String, String)]]
solve [] = pure []
solve (sortOn (length . snd) -> (i, cs):xs) = do
    c <- Set.toList cs
    ((i, c):) <$> solve (xs & each . _2 %~ Set.delete c)

main :: IO ()
main = do
    input <- parseInput $ eachLine inputP
    let candidates = Map.fromListWith Set.intersection
            [ (a, ingredients)
            | (Set.fromList -> ingredients, allergens) <- input
            , a <- allergens
            ]
        possiblyAllergenic = fold candidates
    print $ sum [howMany (`Set.notMember` possiblyAllergenic) ingredients | (ingredients, _) <- input]
    let matching = head $ solve (Map.toList candidates)
    putStrLn $ intercalate "," $ map snd $ sortOn fst matching
