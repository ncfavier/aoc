module Day07 where

import           Data.Map (Map)
import qualified Data.Map as Map

import AOC

type Rule = (String, [(String, Int)])

rule :: Parser Rule
rule = (,) <$> word <> " " <> word <* " bags contain " <*> contains <* "." where
    contains = otherBags `sepBy1` ", " <||> [] <$ "no other bags"
    otherBags = flip (,) <$> decimal <* " " <*> word <> " " <> word <* " bag" <* optional "s"

main = do
    rules <- Map.fromList <$> parseInputLines rule
    let k `leadsTo` target = notNull
            [ () | (k', _) <- rules Map.! k
                 , k' == target || k' `leadsTo` target ]
        countBagsIn k = sum [n * (countBagsIn k' + 1) | (k', n) <- rules Map.! k]
    print $ howMany (`leadsTo` "shiny gold") (Map.keys rules)
    print $ countBagsIn "shiny gold"
