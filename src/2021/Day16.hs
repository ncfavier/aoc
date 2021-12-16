module Day16 where

import AOC

data Packet a = Packet Int (Payload a)
data Payload a = Literal a | Operator OType [Packet a]
data OType = OSum | OProduct | OMinimum | OMaximum | OGreaterThan | OLessThan | OEqual

format = concat <$> many (toBits 4 . digitToInt <$> hexDigitChar) <* newline

int l = fromBits <$> takeP l

packet = do
  version <- int 3
  typeID <- int 3
  payload <- case typeID of
    4 -> literal
    _ -> operator typeID
  pure (Packet version payload)

literal = do
  value <- fromBits <$> (concat <$> many (single True *> takeP 4)) <> (single False *> takeP 4)
  pure (Literal value)

operator typeID = do
  lengthType <- anySingle
  packets <- if lengthType then do
    length <- int 11
    count length packet
  else do
    length <- int 15
    alt . parseMaybe (many packet) =<< takeP length
  pure (Operator otype packets)
  where
    otype = case typeID of
      0 -> OSum
      1 -> OProduct
      2 -> OMinimum
      3 -> OMaximum
      5 -> OGreaterThan
      6 -> OLessThan
      7 -> OEqual

sumVersions :: Packet a -> Int
sumVersions (Packet v p) = v + sumVersions' p
  where
    sumVersions' (Literal _) = 0
    sumVersions' (Operator _ ps) = sum (sumVersions <$> ps)

eval :: Packet Integer -> Integer
eval (Packet _ p) = eval' p
  where
    eval' (Literal v) = v
    eval' (Operator t ps) = op t (eval <$> ps)
    op OSum = sum
    op OProduct = product
    op OMinimum = minimum
    op OMaximum = maximum
    op OGreaterThan = \[a, b] -> bool 0 1 (a > b)
    op OLessThan    = \[a, b] -> bool 0 1 (a < b)
    op OEqual       = \[a, b] -> bool 0 1 (a == b)

main :: IO ()
main = do
  Just p <- parseMaybe (packet <* skipMany (single False)) <$> parseInput format
  print (sumVersions p)
  print (eval p)
