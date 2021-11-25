module Day12 where

import Lens.Micro.Platform

import AOC

data Ship = Ship { _position  :: Coords
                 , _direction :: Coords }

makeLenses ''Ship

finalDistance :: ASetter' Ship Coords -> Coords -> [(Char, Integer)] -> Integer
finalDistance move _direction = go Ship { _position = (0, 0), .. } where
    go s@Ship{..} = \case
        [] -> manhattan _position
        ('E', n):xs -> go (s & move +~ (n, 0)) xs
        ('W', n):xs -> go (s & move +~ (-n, 0)) xs
        ('S', n):xs -> go (s & move +~ (0, n)) xs
        ('N', n):xs -> go (s & move +~ (0, -n)) xs
        ('L', (`div` 90) -> n):xs -> go (s & direction %~ nTimes ccw n) xs
        ('R', (`div` 90) -> n):xs -> go (s & direction %~ nTimes  cw n) xs
        ('F', n):xs -> go (s & position +~ n `mul` _direction) xs

main = do
    steps <- parseInput (eachLine $ (,) <$> letterChar <*> decimal)
    print $ finalDistance position (1, 0) steps
    print $ finalDistance direction (10, -1) steps
