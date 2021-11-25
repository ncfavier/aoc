{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Day12 where

import AOC hiding (add)

type V3 a = (a, a, a)

(x, y, z) `add` (x', y', z') = (x + x', y + y', z + z')

data Moon = Moon { position :: (V3 Integer), velocity :: (V3 Integer) } deriving (Eq, Show)

px (Moon (x, _, _) (vx, _, _)) = (x, vx)
py (Moon (_, y, _) (_, vy, _)) = (y, vy)
pz (Moon (_, _, z) (_, _, vz)) = (z, vz)

moon :: Parser Moon
moon = between "<" ">" $ (\[x, y, z] -> Moon (x, y, z) (0, 0, 0)) <$> (anySingle *> char '=' *> number) `sepBy` ", "

moonEnergy (Moon (x, y, z) (vx, vy, vz)) = (abs x + abs y + abs z) * (abs vx + abs vy + abs vz)
totalEnergy = sum . map moonEnergy

x `pullsOn` x' | x > x'    = 1
               | x < x'    = -1
               | otherwise = 0

applyGravity ms = [ Moon (x, y, z) (foldl add v [(x' `pullsOn` x, y' `pullsOn` y, z' `pullsOn` z) | Moon (x', y', z') _ <- ms])
                  | (Moon (x, y, z) v, ms) <- pickOne ms]

applyVelocity (Moon (x, y, z) (vx, vy, vz)) = Moon (x + vx, y + vy, z + vz) (vx, vy, vz)

step = map applyVelocity . applyGravity

main = do
    moons <- parseInput $ eachLine moon
    print $ totalEnergy $ iterate step moons !! 1000
    print $ foldl1 lcm [findIndices (((==) `on` map p) moons) (iterate step moons) !! 1 | p <- [px, py, pz]]
