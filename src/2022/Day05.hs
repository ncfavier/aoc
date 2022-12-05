module Day05 where

import AOC

import Data.Vector (Vector)
import qualified Data.Vector as V

type Stacks = Vector String

data Instruction = Move
  { count :: Int
  , from :: Int
  , to :: Int
  } deriving Show

type Mover = Instruction -> Stacks -> Stacks

format = do
  rows <- row `sepEndBy` newline
  let stacks = V.fromList $ catMaybes <$> transpose rows
  labels <* newline
  instructions <- instruction `sepEndBy` newline
  pure (stacks, instructions)
  where
    row = (Just <$> between "[" "]" crate <|> Nothing <$ "   ") `sepBy` " "
    crate = letterChar
    labels = length <$ space <*> many (hlexeme number) <* newline
    instruction = do
      count <- "move " *> number <* " "
      from <- pred <$ "from " <*> number <* " "
      to <- pred <$ "to " <*> number
      pure Move{..}

doMove :: Int -> Int -> Int -> Stacks -> Stacks
doMove count from to stacks = stacks & ix from .~ rest & ix to %~ (top <>)
  where (top, rest) = splitAt count $ stacks ^. ix from

crateMover9000, crateMover9001 :: Mover
crateMover9000 Move{..} = nTimes (doMove 1 from to) count
crateMover9001 Move{..} = doMove count from to

finalCrates :: Mover -> Stacks -> [Instruction] -> String
finalCrates mover stacks instructions = V.toList $ head <$> foldl' (flip mover) stacks instructions

main :: IO ()
main = do
  (stacks, instructions) <- parseInput format
  putStrLn (finalCrates crateMover9000 stacks instructions)
  putStrLn (finalCrates crateMover9001 stacks instructions)
