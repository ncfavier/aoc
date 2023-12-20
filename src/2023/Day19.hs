module Day19 where

import AOC

import Data.IntegerInterval as I
import Data.Map qualified as M

data Pair a = Pair a a deriving (Functor)

format = do
  ws <- M.fromList <$> many (workflow <* newline)
  newline
  ps <- many (part <* newline)
  pure (ws, ps)
  where
    wid = word
    cid = letterChar
    workflow = (,) <$> wid <*> between "{" "}" (rule `sepBy` ",")
    rule = (,) <$> optional (try condition <* ":") <*> wid
    condition = (,,) <$> cid <*> op <*> number
    op = oneOf "<>"
    part = M.fromList <$> between "{" "}" (rating `sepBy` ",")
    rating = (,) <$> cid <* "=" <*> number

liftJust :: Functor f => (a -> f b) -> Maybe a -> f (Maybe b)
liftJust f (Just b) = Just <$> f b

main = do
  (workflows, parts) <- parseInput format
  let
    _ `matches` Nothing = True
    p `matches` Just (c, '<', n) = p M.! c < n
    p `matches` Just (c, '>', n) = p M.! c > n
    accepted = go "in" where
      go "A" _ = True
      go "R" _ = False
      go w p = head [go w' p | (r, w') <- workflows M.! w, p `matches` r]
  print $ sum $ map sum $ filter accepted parts
  let
    range = M.fromList [(c, 1 <=..<= 4000) | c <- "xmas"]
    splitI _ _ i | I.null i = Pair I.empty I.empty
    splitI '<' n i = Pair (I.lowerBound i <=..< Finite n) (Finite n <=..<= I.upperBound i)
    splitI '>' n i = Pair (Finite n <..<= I.upperBound i) (I.lowerBound i <=..<= Finite n)
    countI i | I.null i = 0
             | (Finite l, Finite u) <- (I.lowerBound i, I.upperBound i) = u - l + 1
    acceptedN = go "in" where
      go "A" i = product (countI <$> i)
      go "R" _ = 0
      go w i = go' (workflows M.! w) i where
        go' ((Just (c, t, n), w'):rs) i = go w' yes + go' rs no
          where Pair yes no = i & at c %%~ liftJust (splitI t n)
        go' ((Nothing, w'):_) i = go w' i
  print $ acceptedN range
