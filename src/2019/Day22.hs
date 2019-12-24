{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
module Day22 where

import Data.Semigroup
import GHC.TypeLits hiding (Mod)

import AOC

data Technique = Reverse
               | Cut Integer
               | Increment Integer

technique :: Parser Technique
technique =  Reverse   <$ "deal into new stack"
         <|> Cut       <$ "cut " <*> number
         <|> Increment <$ "deal with increment " <*> number

-- When `n` is prime, this represents the field of integers modulo `n`.
data Mod (n :: Nat) = Mod Integer

instance KnownNat n => Num (Mod n) where
    fromInteger i = x
        where x = Mod (i `mod` natVal x)
    Mod a + Mod b = fromInteger (a + b)
    Mod a * Mod b = fromInteger (a * b)
    negate (Mod a) = fromInteger (negate a)
    abs = undefined
    signum = undefined

instance KnownNat n => Fractional (Mod n) where
    -- This is only true if `n` is prime, which is assumed here.
    recip x = x ^ (natVal x - 2)
    fromRational = undefined

instance KnownNat n => Show (Mod n) where
    show (Mod i) = show i

-- `a :+ b` represents the linear polynomial `aX + b`.
data Linear a = a :+ a

-- Linear polynomials form a monoid under left-to-right composition.
instance Num a => Semigroup (Linear a) where
    (a :+ b) <> (c :+ d) = (c * a) :+ (c * b + d) -- c(aX + b) + d = caX + cb + d
instance Num a => Monoid (Linear a) where
    mempty = 1 :+ 0

-- Evaluate `ax + b = ?`
($@) :: Num a => Linear a -> a -> a
(a :+ b) $@ x = a * x + b

-- Solve `a? + b = y`
($?) :: Fractional a => Linear a -> a -> a
(a :+ b) $? y = (y - b) / a

-- Represents a shuffling technique as an affine transformation (linear polynomial)
-- of a card's position, modulo `n`.
techniqueToLinear :: KnownNat n => Technique -> Linear (Mod n)
techniqueToLinear Reverse       = (-1) :+ (-1)
techniqueToLinear (Cut k)       = 1 :+ fromInteger (-k)
techniqueToLinear (Increment k) = fromInteger k :+ 0

main :: IO ()
main = do
    Just techniques <- parseMaybe (parseLines technique) <$> getContents
    let shuffle :: KnownNat n => Linear (Mod n)
        shuffle = foldMap techniqueToLinear techniques
    print $ shuffle @10007 $@ 2019
    print $ 101741582076661 `stimes` shuffle @119315717514047 $? 2020
