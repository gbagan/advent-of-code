module AOC.Util where
import           AOC.Prelude
import           AOC.List (count)
import           Relude.Unsafe ((!!))
import qualified Data.HashMap.Strict as HMap
import           AOC.V2 (V2(..))

freqs :: Hashable a => [a] -> HashMap a Int
freqs = HMap.fromListWith (+) . map (,1)
{-# INLINE freqs #-}

majority :: (a -> Bool) -> [a] -> Bool
majority f l = 2 * count f l >= length l

median :: Ord a => [a] -> a
median l = sort l !! (length l `div` 2)

clamp :: Ord a => (a, a) -> a -> a
clamp (l, u) = max l . min u
{-# INLINE clamp #-}

listTo2dMap :: [[a]] -> HashMap (V2 Int) a
listTo2dMap l =
    HMap.fromList
        [(V2 i j, v)
        | (i, row) <- zip [0..] l
        , (j, v) <- zip [0..] row
        ]

binToInt :: [Bool] -> Int
binToInt = foldl' (\acc x -> acc * 2 + fromEnum x) 0

-- return the double of the polygon area
shoelaceFormula :: Num a => [V2 a] -> a
shoelaceFormula points = abs . sum $ zipWith go points (drop 1 points ++ points)
    where go (V2 x y) (V2 x' y') = x * y' - x' * y