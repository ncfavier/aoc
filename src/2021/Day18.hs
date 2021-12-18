module Day18 where

import AOC hiding (left, right)

import Data.Data.Lens
import Data.Data

data Snailfish = Pure { _it :: Int } | Pair { _left :: Snailfish, _right :: Snailfish } deriving (Data, Typeable)
instance Show Snailfish where
  show (Pure n) = show n
  show (Pair a b) = "[" ++ show a ++ "," ++ show b ++ "]"

makeLenses ''Snailfish

format = eachLine snailfish
  where
    snailfish = pair <|> single
    pair = Pair <$ "[" <*> snailfish <* "," <*> snailfish <* "]"
    single = Pure <$> decimal

magnitude (Pure n) = n
magnitude (Pair a b) = 3 * magnitude a + 2 * magnitude b

splitSingle n = Pair (Pure $ n `div` 2) (Pure $ (n + 1) `div` 2)
explode p l r d (Pair (Pure a) (Pure b)) | d >= 4 = Just (l, r, p, a, b)
explode p l r d (Pair a b) = explode (p . left) l (Just $ p . right) (d + 1) a <|> explode (p . right) (Just $ p . left) r (d + 1) b
explode _ _ _ _ _ = Nothing
split p (Pair a b) = split (p . left) a <|> split (p . right) b
split p (Pure n) | n >= 10 = Just (p, splitSingle n)
                 | otherwise = Nothing
final n = case explode id Nothing Nothing 0 n of
  Just (l, r, p, a, b) -> final $ n & fromMaybe ignored l . partsOf biplate . _last +~ a
                                 & fromMaybe ignored r  . partsOf biplate . _head +~ b
                                 & p .~ Pure 0
  Nothing -> case split id n of
    Just (p, ab) -> final $ n & p .~ ab
    Nothing -> n

add a b = final $ Pair a b

main :: IO ()
main = do
  input <- parseInput format
  let number = foldl1 add input
  print $ magnitude number
  print $ maximum [magnitude (add a b) | a <- input, b <- input]
