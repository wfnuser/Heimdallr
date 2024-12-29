{-# LANGUAGE Safe #-}

module Math.ModularArith.Basic
  ( extendedGCD,
  )
where

-- | Extended Euclidean Algorithm
-- Returns (x, y, d) where d = gcd(a,b) and ax + by = d
extendedGCD :: Integer -> Integer -> (Integer, Integer, Integer)
extendedGCD a b = go a b 1 0 0 1
  where
    go r0 r1 s0 s1 t0 t1
      | r1 == 0 = (s0, t0, r0)
      | otherwise = go r1 r2 s1 s2 t1 t2
      where
        q = r0 `div` r1
        r2 = r0 - q * r1
        s2 = s0 - q * s1
        t2 = t0 - q * t1