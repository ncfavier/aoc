module Day07 where

import           Data.Map (Map)
import qualified Data.Map as Map

import AOC

type Rule = (String, [(String, Int)])

rule :: Parser Rule
rule = (,) <$> word <> " " <> word <* " bags contain " <*> contains <* "." where
    contains = [] <$ "no other bags" <||> otherBags `sepBy` ", "
    otherBags = flip (,) <$> decimal <* " " <*> word <> " " <> word <* " bag" <* optional "s"

main = do
    rules <- Map.fromList <$> parseInputLines rule
    let leadsToShinyGold k = not $ null [k' | (k', _) <- rules Map.! k, k' == "shiny gold" || leadsToShinyGold k']
        countBagsIn k = sum [n * countBagsIn k' + n | (k', n) <- rules Map.! k]
    print $ howMany leadsToShinyGold (Map.keys rules)
    print $ countBagsIn "shiny gold"
