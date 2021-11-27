module Day16 where

import AOC

type Field = (String, Int -> Bool)

type Ticket = [Int]

field :: Parser Field
field = do
    name <- some (letterChar <|> char ' ')
    ": "
    ranges <- ((,) <$> decimal <* "-" <*> decimal) `sepBy` " or "
    let valid n = or [inRange r n | r <- ranges]
    pure (name, valid)

ticket :: Parser Ticket
ticket = decimal `sepBy` ","

document :: Parser ([Field], Ticket, [Ticket])
document = do
    fields <- field `endBy` newline
    "\nyour ticket:\n"
    myTicket <- ticket <* newline
    "\nnearby tickets:\n"
    nearbyTickets <- ticket `endBy` newline
    pure (fields, myTicket, nearbyTickets)

main :: IO ()
main = do
    (fields, myTicket, nearbyTickets) <- parseInput document
    let invalid n = and [not (valid n) | (_, valid) <- fields]
    print $ sum $ nearbyTickets ^.. each . each . filtered invalid
    let columns = transpose (myTicket : filter (not . any invalid) nearbyTickets)
        candidates = [(i, [name | (name, valid) <- fields, all valid vs]) | i <- [0..] | vs <- columns]
        solve [] = pure []
        solve (sortOn (length . snd) -> (i, fs):cs) = do
            f <- fs
            ((i, f):) <$> solve (cs & each . _2 %~ delete f)
    print $ product [myTicket !! i | (i, f) <- head (solve candidates), "departure " `isPrefixOf` f]
