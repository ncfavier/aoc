module Day21 where

import AOC

import Data.Map qualified as M

data Expr a = Int Int | Var a | Op (Expr a) Char (Expr a)
  deriving Functor

format = M.fromList <$> eachLine monkey where
  name = count 4 letterChar
  monkey = (,) <$> name <* ": " <*> (Int <$> number <|> Op <$> (Var <$> name) <* space <*> anySingle <* space <*> (Var <$> name))

eval :: Expr Int -> Int
eval (Int n) = n
eval (Var x) = x
eval (Op a '+' b) = eval a + eval b
eval (Op a '-' b) = eval a - eval b
eval (Op a '*' b) = eval a * eval b
eval (Op a '/' b) = eval a `div` eval b

-- | Solve `e = 0` for the first variable encountered.
solve :: Expr a -> Expr a
solve = fromMaybe (error "no variables") . go (Int 0) where
  go _ (Int _) = Nothing
  go y (Var _) = Just y
  go y (Op a '+' b) = go (Op y '-' b) a <|> go (Op y '-' a) b
  go y (Op a '-' b) = go (Op y '+' b) a <|> go (Op a '-' y) b
  go y (Op a '*' b) = go (Op y '/' b) a <|> go (Op y '/' a) b
  go y (Op a '/' b) = go (Op y '*' b) a <|> go (Op a '/' y) b

main :: IO ()
main = do
  jobs <- parseInput format
  let exprs = löb $ flip M.mapWithKey jobs \ name expr go -> case expr of
        Int n | name == "humn" -> Var "humn"
              | otherwise -> Int n
        Op (Var a) o (Var b) -> Op (go M.! a) o (go M.! b)
      root@(Op a _ b) = exprs M.! "root"
      Int humn = jobs M.! "humn"
  print (eval $ humn <$ root)
  print (eval $ error <$> solve (Op a '-' b))
