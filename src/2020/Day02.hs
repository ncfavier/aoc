module Day02 where

import AOC

policiesAndPasswords :: Parser (Int, Int, Char, String)
policiesAndPasswords = (,,,) <$> decimal <* "-" <*> decimal <* space <*> letterChar <* ": " <*> many letterChar

main = do
    input <- parseInputLines policiesAndPasswords
    print $ input & count \(from, to, letter, password) ->
        let c = count (== letter) password in
        inRange (from, to) c
    print $ input & count \(from, to, letter, password) ->
        let match i = password !! pred i == letter in
        match from /= match to
