module AOC.Number where

import AOC.Prelude

-- extgcd a b
-- Return the gcd of a and b, along with x and y such that
-- gcd(a,b) = ax + by
extgcd :: Integral a => a -> a -> (a, a, a)
extgcd 0 0 = error "extgcd 0 0 is undefined"
extgcd a 0 = (1,0,a)
extgcd a b = (x, c-q*x, y) where 
    (q,r) = a `eDivMod` b
    (c,x,y) = extgcd b r

-- minv a n
-- Returns the modular inverse of a mod n. Assumes that a and n are coprime.
modularInverse a n =
  | g == 1    = Just $ a*x + n*y
  | otherwise = Nothing
  where
    (x, y, g) = extgcd a n