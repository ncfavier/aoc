module Day07 where

import AOC

data Card = C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | CT | CJ | CQ | CK | CA deriving (Eq, Ord, Show)

format = eachLine do (,) <$> lexeme (count 5 card) <*> lexeme number
  where
    card = C2 <$ char '2' <|> C3 <$ char '3' <|> C4 <$ char '4' <|> C5 <$ char '5' <|> C6 <$ char '6' <|> C7 <$ char '7' <|> C8 <$ char '8' <|> C9 <$ char '9' <|> CT <$ char 'T' <|> CJ <$ char 'J' <|> CQ <$ char 'Q' <|> CK <$ char 'K' <|> CA <$ char 'A'

compareJ CJ CJ = EQ
compareJ CJ _ = LT
compareJ _ CJ = GT
compareJ a b = compare a b

compareList comp a b = mconcat (zipWith comp a b)

handType joker h = _head +~ j $ reverse $ sort $ map length $ ([]:) $ group $ sort $ h'
  where (length -> j, h') = partition (maybe (const False) (==) joker) h

main :: IO ()
main = do
  hands <- parseInput format
  print $ sum $ zipWith (\ i (_, bid) -> i * bid) [1..] $ sortBy ((comparing (handType Nothing) <> compare) `on` fst) hands
  print $ sum $ zipWith (\ i (_, bid) -> i * bid) [1..] $ sortBy ((comparing (handType (Just CJ)) <> compareList compareJ) `on` fst) hands
