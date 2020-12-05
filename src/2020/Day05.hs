module Day05 where

import           Data.Set (Set)
import qualified Data.Set as Set

import AOC

main = do
    ids <- Set.fromList <$> parseInputLines do
        setInput =<< many ('0' <$ oneOf "LF" <|> '1' <$ oneOf "RB")
        binary
    print (Set.findMax ids)
    print $ head [id | id <- [Set.findMin ids..]
                     , id `Set.notMember` ids]
