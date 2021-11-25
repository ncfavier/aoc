module Day07 where

import           Data.Map (Map)
import qualified Data.Map as Map

import AOC

rule = (,) <$> word <> " " <> word <* " bags contain " <*> contents <* "." where
    contents = otherBags `sepBy1` ", " <||> [] <$ "no other bags"
    otherBags = flip (,) <$> decimal <* " " <*> word <> " " <> word <* " bag" <* optional "s"

main = do
    rules <- Map.fromList <$> parseInput (eachLine rule)
    let bags = Map.keysSet rules
        contains target = fixMem bags \containsTarget k ->
            or [k' == target || containsTarget k' | (k', _) <- rules Map.! k]
        countBagsIn = fixMem bags \countBagsIn k ->
            sum [n * (countBagsIn k' + 1) | (k', n) <- rules Map.! k]
    print $ howMany (contains "shiny gold") bags
    print $ countBagsIn "shiny gold"
