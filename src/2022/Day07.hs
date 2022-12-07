module Day07 where

import AOC

import Data.Map qualified as M
import System.FilePath

data File = File Int | Dir (Map FilePath File)

data Command = Cd FilePath | Ls (Map FilePath File)

format = many ("$ " *> (ls <|> cd))
  where
    cd = Cd <$ "cd " <*> restOfLine
    ls = Ls . M.fromList <$ "ls" <* newline <*> many (dir <|> file)
    dir = (,Dir mempty) <$ "dir " <*> restOfLine
    file = flip (,) <$> (File <$> number) <* space <*> restOfLine

cd :: FilePath -> FilePath -> FilePath
cd "/" = const "/"
cd ".." = takeDirectory
cd d = (</> d)

ls :: FilePath -> Map FilePath File -> File -> File
ls path entries = go dirs where
  "/":dirs = splitDirectories path
  go (d:ds) (Dir entries) = Dir (M.adjust (go ds) d entries)
  go [] (Dir _) = Dir entries

reconstructRoot :: [Command] -> File
reconstructRoot = go "/" (Dir mempty) where
  go p root (Cd d:cmds) = go (cd d p) root cmds
  go p root (Ls files:cmds) = go p (ls p files root) cmds
  go _ root [] = root

-- (total size, all dir sizes)
getSizes :: File -> (Int, [Int])
getSizes (File size) = (size, [])
getSizes (Dir entries) = (size, size:sizes) where
  (sum -> size, concat -> sizes) = unzip $ toList $ getSizes <$> entries

main :: IO ()
main = do
  commands <- parseInput format
  let (total, sizes) = getSizes $ reconstructRoot commands
      mustFree = total - 70000000 + 30000000
  print $ sum $ filter (<= 100000) sizes
  print $ minimum $ filter (>= mustFree) sizes
