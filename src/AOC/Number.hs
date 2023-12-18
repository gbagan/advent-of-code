module AOC.Number where

import AOC.Prelude

-- Return (g, x, y) such that g is the gcd of a and b
-- and ax + by = gcd(a,b)
extgcd :: Integral a => a -> a -> (a, a, a)
extgcd 0 0 = error "extgcd 0 0 is undefined"
extgcd a 0 = (a, 1, 0)
extgcd a b = (g, y, x-q*y) where 
    (q,r) = a `divMod` b
    (g, x, y) = extgcd b r

{-# SPECIALISE extgcd :: Integer -> Integer -> (Integer, Integer, Integer) #-}

-- Return the modular inverse of a mod n.
modularInverse :: Integral a => a -> a -> Maybe a
modularInverse a n
    | g == 1    = Just $ x `mod` n
    | otherwise = Nothing
    where (g, x, _) = extgcd a n

{-# SPECIALISE modularInverse :: Integer -> Integer -> Maybe Integer #-}

{-
chineseRemainder :: Integral a => [(a, a)] -> Maybe (a, a)
chineseRemainder = foldlM go (0, 1) where
    go (r1, m1) (r2, m2) = do
        im2 <- modularInverse m2 m1
        let r = r2 + (r1 - r2) * m2 * im2
        let m = m1 * m2
        return (r `mod` m, m)
-}

-- Given a list of (ri, mi)
-- returns a tuple (q, m) where {q + j m | j in Z} is the set of solutions
-- of the equations x = ri (mod mi)
-- It is not necessary that all mi are pairwise coprime
-- returns Nothing if there is no solution
chineseRemainder :: Integral a => [(a, a)] -> Maybe (a, a)
chineseRemainder = foldlM go (0, 1) where
    go (a, m) (b, n) = do
        let (g, u, v) = extgcd m n
        guard $ (a - b) `mod` g == 0
        let x = (a * v * n + b * u * m) `div` g
        let m' = (m * n) `div` g
        return (x `mod` m', m')

{-# SPECIALISE chineseRemainder :: [(Integer, Integer)] -> Maybe (Integer, Integer) #-}