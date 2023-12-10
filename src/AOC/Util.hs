module AOC.Util where
import           AOC.Prelude
import           Relude.Unsafe ((!!))
import           Data.List (maximum)
import qualified Data.HashMap.Strict as HMap
import           AOC.V2 (V2(..))

type Point = (Int, Int)

-- functions on lists

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f
{-# INLINE count #-}

slice :: Int -> Int -> [a] -> [a]
slice start end = take (end - start + 1) . drop start
{-# INLINE slice #-}

freqs :: Hashable a => [a] -> HashMap a Int
freqs = HMap.fromListWith (+) . map (,1)
{-# INLINE freqs #-}

allUnique :: Ord a => [a] -> Bool
allUnique xs = length (ordNub xs) == length xs
{-# INLINE allUnique #-}

maximumDef :: Ord a => a -> [a] -> a
maximumDef def [] = def
maximumDef _ l = maximum l

drop1 :: [a] -> [a]
drop1 [] = []
drop1 (_:xs) = xs

takeEnd :: Int -> [a] -> [a]
takeEnd i l
    | i <= 0 = []
    | otherwise = f l (drop i l)
    where f (_:xs) (_:ys) = f xs ys
          f xs _ = xs

wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy f s = case dropWhile f s of
    [] -> []
    x:xs -> (x:w) : wordsBy f (drop1 z)
        where (w,z) = break f xs

average :: [Int] -> Double
average xs = realToFrac (sum xs) / genericLength xs
{-# INLINE average #-}

majority :: (a -> Bool) -> [a] -> Bool
majority f l = 2 * count f l >= length l

median :: Ord a => [a] -> a
median l = sort l !! (length l `div` 2)

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct l1 l2 = (,) <$> l1 <*> l2
{-# INLINE cartesianProduct #-}

clamp :: Ord a => (a, a) -> a -> a
clamp (l, u) = max l . min u
{-# INLINE clamp #-}

flattenWithIndex :: [[a]] -> [(Int, Int, a)]
flattenWithIndex l =
    [(i, j, v)
    | (i, row) <- zip [0..] l
    , (j, v) <- zip [0..] row
    ]

listTo2dMap :: [[a]] -> HashMap (Int, Int) a
listTo2dMap l =
    HMap.fromList
        [((i, j), v)
        | (i, row) <- zip [0..] l
        , (j, v) <- zip [0..] row
        ]

listTo2dMap' :: [[a]] -> HashMap (V2 Int) a
listTo2dMap' l =
    HMap.fromList
        [(V2 i j, v)
        | (i, row) <- zip [0..] l
        , (j, v) <- zip [0..] row
        ]

adjacentPoints :: Point -> [Point]
adjacentPoints (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

kingAdjacentPoints :: Point -> [Point]
kingAdjacentPoints (x, y) = adjacentPoints (x, y) ++ [(x-1, y-1), (x+1, y-1), (x-1, y+1), (x+1, y+1)]

kingAdjacentPoints' :: V2 Int -> [V2 Int]
kingAdjacentPoints' (V2 x y) = kingAdjacentPoints (x, y) <&> uncurry V2


binToInt :: [Bool] -> Int
binToInt = foldl' (\acc x -> acc * 2 + fromEnum x) 0
{-# INLINE binToInt #-}

