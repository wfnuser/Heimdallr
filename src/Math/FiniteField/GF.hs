{-# LANGUAGE Safe #-}

module Math.FiniteField.GF
  ( GF (..),
    mkGF,
    add,
    sub,
    mul,
    inv,
    pow,
  )
where

import Data.Maybe (fromMaybe)
import Math.ModularArith.Basic (extendedGCD)

-- | Galois Field element GF(p)
data GF = GF
  { -- | The value in the field
    value :: Integer,
    -- | The characteristic (prime p)
    char :: Integer
  }
  deriving (Eq)

instance Show GF where
  show (GF v p) = show v ++ " (mod " ++ show p ++ ")"

-- | Create a GF(p) element
mkGF :: Integer -> Integer -> Maybe GF
mkGF p x
  | p < 2 = Nothing -- p must be at least 2
  | otherwise = Just $ GF (x `mod` p) p

-- | Add two elements in GF(p)
add :: GF -> GF -> Maybe GF
add (GF a p1) (GF b p2)
  | p1 /= p2 = Nothing
  | otherwise = Just $ GF ((a + b) `mod` p1) p1

-- | Subtract two elements in GF(p)
sub :: GF -> GF -> Maybe GF
sub (GF a p1) (GF b p2)
  | p1 /= p2 = Nothing
  | otherwise = Just $ GF ((a - b) `mod` p1) p1

-- | Multiply two elements in GF(p)
mul :: GF -> GF -> Maybe GF
mul (GF a p1) (GF b p2)
  | p1 /= p2 = Nothing
  | otherwise = Just $ GF ((a * b) `mod` p1) p1

-- | Find multiplicative inverse in GF(p)
inv :: GF -> Maybe GF
inv (GF a p)
  | gcd a p /= 1 = Nothing
  | otherwise = Just $ GF (x `mod` p) p
  where
    (x, _, _) = extendedGCD a p

-- | Exponentiation in GF(p)
pow :: GF -> Integer -> Maybe GF
pow (GF a p) n
  | n < 0 = do
    inv_a <- inv (GF a p)
    pow inv_a (- n)
  | n == 0 = Just $ GF 1 p
  | n == 1 = Just $ GF a p
  | otherwise = Just $ GF (powerMod a n p) p

-- | Fast modular exponentiation
powerMod :: Integer -> Integer -> Integer -> Integer
powerMod base exp modulus = go base exp 1
  where
    go _ 0 acc = acc
    go b e acc
      | odd e = go (b * b `mod` modulus) (e `div` 2) (acc * b `mod` modulus)
      | otherwise = go (b * b `mod` modulus) (e `div` 2) acc