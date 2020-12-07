module Day07 where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import AOC

type Rule = (String, [(String, Int)])

rule :: Parser Rule
rule = (,) <$> word <> " " <> word <* " bags contain " <*> contents <* "." where
    contents = otherBags `sepBy1` ", " <||> [] <$ "no other bags"
    otherBags = flip (,) <$> decimal <* " " <*> word <> " " <> word <* " bag" <* optional "s"

main = do
    rules <- Map.fromList <$> parseInputLines rule
    let bags = Map.keysSet rules
        contains target = fixMem bags \contains k ->
            notNull [ () | (k', _) <- rules Map.! k
                         , k' == target || contains k' ]
        countBagsIn = fixMem bags \countBagsIn k ->
            sum [n * (countBagsIn k' + 1) | (k', n) <- rules Map.! k]
    print $ howMany (contains "shiny gold") bags
    print $ countBagsIn "shiny gold"
