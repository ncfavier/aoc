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
    input <- Map.fromList <$> parseInputLines rule
    let leadsToShinyGold k =
            let vs = input Map.! k in
            not (null [() | ("shiny gold", _) <- vs]) ||
            any (leadsToShinyGold . fst) vs
        countBagsIn k =
            let vs = input Map.! k in
            let s = sum (map snd vs) in
            let s' = sum [n * countBagsIn k' | (k', n) <- vs] in
            s + s'
    print $ length $ Map.filterWithKey (\k v -> leadsToShinyGold k) input
    print $ countBagsIn "shiny gold"
