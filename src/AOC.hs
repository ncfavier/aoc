{-# LANGUAGE TypeFamilies #-}
module AOC ( module AOC
           , module Control.Applicative
           , module Control.Monad
           , module Control.Arrow
           , module Data.Ord
           , module Data.Char
           , module Data.List
           , module Data.List.Split
           , module Data.Void
           , module Data.Function
           , module Text.Megaparsec
           , module Text.Megaparsec.Char
           , module Text.Megaparsec.Char.Lexer
           ) where

import Control.Applicative hiding (some, many)
import Control.Monad
import Control.Arrow
import Data.Ord
import Data.Char
import Data.List
import Data.List.Split (splitOn, chunksOf)
import Data.Void
import Data.Function
import Text.Megaparsec hiding (count)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)

type Parser = Parsec Void String

parseLines p = many (p <* eol)

type Coords = (Integer, Integer)

add :: Coords -> Coords -> Coords
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

ccw, cw :: Coords -> Coords
ccw (x, y) = (y, -x)
cw  (x, y) = (-y, x)

flatten rows = [ ((x, y), c)
               | (y, row) <- zip [0..] rows
               , (x, c)   <- zip [0..] row
               ]
