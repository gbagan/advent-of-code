module AOC.Number where

import           AOC.Prelude
import qualified Data.HashMap.Strict as Map

toInteger :: Integral a => a -> Integer
toInteger = fromIntegral

toDouble :: Integral a => a -> Double
toDouble = fromIntegral

toFrac :: Integral a => a-> Rational
toFrac = fromIntegral

power :: Integral b => (a -> a -> a) -> a -> b -> a
power mul x n
    | n <= 0 = error "power: exponent must be positive"
    | otherwise = go n
    where
    square y = y `mul` y
    go 1             = x
    go i | even i    = square (go (i `quot` 2))
         | otherwise = square (go (i `quot` 2)) `mul` x

{-# INLINE power #-}

-- return an integer x such that m = base ^ x % modulo
-- assume that modulo is prime
discreteLogarithm :: Integer -> Integer -> Integer -> Maybe Integer
discreteLogarithm base modulo m =
    let n = ceiling . sqrt $ toDouble modulo
        mul x y = x * y `rem` modulo
        table = Map.fromList $ zip (iterate' (mul base) 1) [0..n-1]
        c = power mul base (n * (modulo - 2))
    in listToMaybe $ mapMaybe (\(i, y) ->
        [i * n + z | z <- Map.lookup y table]
        ) (zip [0..n-1] (iterate' (mul c) m))

{-# INLINE discreteLogarithm #-}

-- Return (g, x, y) such that g is the gcd of a and b
-- and ax + by = gcd(a,b)
extgcd :: Integral a => a -> a -> (a, a, a)
extgcd 0 0 = error "extgcd 0 0 is undefined"
extgcd a 0 = (a, 1, 0)
extgcd a b = (g, y, x-q*y) where 
    (q,r) = a `divMod` b
    (g, x, y) = extgcd b r

{-# INLINE extgcd #-}

-- Return the modular inverse of a mod n.
modularInverse :: Integral a => a -> a -> Maybe a
modularInverse a n
    | g == 1    = Just $ x `mod` n
    | otherwise = Nothing
    where (g, x, _) = extgcd a n

{-# INLINE modularInverse #-}

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

{-# INLINE chineseRemainder #-}