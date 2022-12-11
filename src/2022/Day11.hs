module Day11 where

import AOC

import Data.Map qualified as M
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

data Monkey = Monkey
  { items :: Seq Int
  , activity :: Int
  , operation :: Expr
  , test :: Int, ifTrue :: Int, ifFalse :: Int
  } deriving Show

data Expr = Add Expr Expr | Mul Expr Expr | Old | Int Int deriving Show

evalExpr :: Int -> Expr -> Int
evalExpr old (Add a b) = evalExpr old a + evalExpr old b
evalExpr old (Mul a b) = evalExpr old a * evalExpr old b
evalExpr old Old = old
evalExpr _ (Int n) = n

format = M.fromList <$> monkey `sepBy` newline where
  monkey = do
    n <- "Monkey " *> number <* ":" <* newline
    items <- Seq.fromList <$> (space *> "Starting items: " *> number `sepBy` ", " <* newline)
    operation <- space *> "Operation: new = " *> expr <* newline
    test <- space *> "Test: divisible by " *> number <* newline
    ifTrue <- space *> "If true: throw to monkey " *> number <* newline
    ifFalse <- space *> "If false: throw to monkey " *> number <* newline
    pure (n, Monkey{activity = 0, ..})
  expr = makeExprParser (Old <$ hlexeme "old" <|> Int <$> hlexeme number) [[binary "*" Mul], [binary "+" Add]]
  binary name f = InfixL (f <$ hlexeme name)

runMonkeys :: Int -> (Int -> Int) -> State (Map Int Monkey) ()
runMonkeys rounds boredom = do
  monkeys <- get
  replicateM_ rounds do
    for_ (M.keys monkeys) \ i -> do
      m@Monkey{..} <- gets (M.! i)
      for_ items \ old -> do
        let new = boredom $ evalExpr old operation
            throwTo = if new `mod` test == 0 then ifTrue else ifFalse
        ix throwTo %= \ m'@Monkey{items} -> m' { items = items :> new}
      ix i .= m { items = mempty, activity = activity + length items }

main :: IO ()
main = do
  monkeys <- parseInput format
  let monkeyBusiness rounds boredom = product $ take 2 $ sortDesc $ activity <$> M.elems finalMonkeys
        where finalMonkeys = execState (runMonkeys rounds boredom) monkeys
  print $ monkeyBusiness 20 (`div` 3)
  let modulo = product $ test <$> M.elems monkeys
  print $ monkeyBusiness 10000 (`mod` modulo)
