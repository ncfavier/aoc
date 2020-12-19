module Day19 where

import Data.Map qualified as Map
import Text.ParserCombinators.ReadP qualified as R

import AOC

data Rule = Char Char | Sub [[Int]]

rule :: Parser (Int, Rule)
rule = do
    n <- decimal <* lexeme' ":"
    r <- Char <$> lexeme' ("\"" *> anySingle <* "\"")
      <|> Sub <$> some (lexeme' decimal) `sepBy` lexeme' "|"
    pure (n, r)

inputP :: Parser (Map Int Rule, [String])
inputP = do
    rules <- Map.fromList <$> rule `endBy` newline
    newline
    msgs <- many letterChar `endBy` newline
    pure (rules, msgs)

parser :: Map Int Rule -> R.ReadP ()
parser rules = go Map.! 0 *> R.eof where
    go = f <$> rules
    f (Char c) = () <$ R.char c
    f (Sub s)  = asum (map (traverse_ (go Map.!)) s)

main :: IO ()
main = do
    (rules, msgs) <- parseInput inputP
    let rules' = rules & Map.insert 8 (Sub [[42], [42, 8]])
                       & Map.insert 11 (Sub [[42, 31], [42, 11, 31]])
    for_ [rules, rules'] \rules -> do
        let p = parser rules
        print $ howMany (notNull . R.readP_to_S p) msgs
