module Day25 where

import GHC.TypeNats hiding (Mod)

import AOC

m = 20201227

subject = 7

crack :: forall m. KnownNat m => Mod m -> Natural
crack pk = discreteLogarithm cg rt x where
    Just cg = cyclicGroup
    Just rt = isPrimitiveRoot @Integer cg subject
    Just x  = isMultElement pk

main :: IO ()
main = case someNatVal m of
    SomeNat (_ :: _proxy m) -> do
        [card, door] <- parseInput (eachLine $ decimal @(Mod m))
        print $ getVal $ door ^% crack card
