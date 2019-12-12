{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import Data.List
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

type Parser = Parsec Void String

type V3 a = (a, a, a)

(x, y, z) `add` (x', y', z') = (x + x', y + y', z + z')

data Moon = Moon { position :: (V3 Integer), velocity :: (V3 Integer) } deriving Show

number = signed (return ()) decimal
parseLines p = many (p <* eol)

moons :: Parser [Moon]
moons = parseLines $ between "<" ">" $ (\[x, y, z] -> Moon (x, y, z) (0, 0, 0)) <$> (anySingle *> char '=' *> number) `sepBy` ", "

moonEnergy (Moon (x, y, z) (vx, vy, vz)) = (abs x + abs y + abs z) * (abs vx + abs vy + abs vz)
totalEnergy = sum . map moonEnergy

pickOne xs = [(x, l ++ r) | (l, x:r) <- zip (inits xs) (tails xs)]

pull x {->-} x' | x < x'    = 1
                | x > x'    = -1
                | otherwise = 0

applyGravity ms = [ Moon (x, y, z) (v `add` foldl1 add [(pull x x', pull y y', pull z z') | Moon (x', y', z') _ <- ms])
                  | (Moon (x, y, z) v, ms) <- pickOne ms]

applyVelocity (Moon (x, y, z) (vx, vy, vz)) = Moon (x + vx, y + vy, z + vz) (vx, vy, vz)

step = map applyVelocity . applyGravity

main = do
    Just moons <- parseMaybe moons <$> readFile "input12"
    print $ totalEnergy $ iterate step moons !! 1000
