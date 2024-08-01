module AOC.List where

import           AOC.Prelude
import           Data.List ((!!), maximum, minimum, maximumBy, minimumBy)
import qualified Data.HashMap.Strict as HMap

allUnique :: Ord a => [a] -> Bool
allUnique xs = length (ordNub xs) == length xs
{-# INLINE allUnique #-}

minimumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> a
minimumOn f = minimumBy (comparing f)
{-# INLINE minimumOn #-}

maximumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> a
maximumOn f = maximumBy (comparing f)
{-# INLINE maximumOn #-}

minimumMaybe :: Ord a => [a] -> Maybe a
minimumMaybe [] = Nothing
minimumMaybe l = Just $ minimum l
{-# INLINE minimumMaybe #-}

maximumDef :: Ord a => a -> [a] -> a
maximumDef def [] = def
maximumDef _ l = maximum l
{-# INLINE maximumDef #-}

minimumDef :: Ord a => a -> [a] -> a
minimumDef def [] = def
minimumDef _ l = minimum l
{-# INLINE minimumDef #-}

groupOn :: Eq k => (a -> k) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on2` f)
    where (.*.) `on2` g = \x -> let fx = f x in \y -> fx .*. g y

drop1 :: [a] -> [a]
drop1 [] = []
drop1 (_:xs) = xs

takeEnd :: Int -> [a] -> [a]
takeEnd i l
    | i <= 0 = []
    | otherwise = f l (drop i l)
    where f (_:xs) (_:ys) = f xs ys
          f xs _ = xs

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen f l = case ys of
    [] -> [xs]
    (_:zs) -> xs : splitWhen f zs
    where (xs,ys) = break f l

wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy f s = case dropWhile f s of
    [] -> []
    x:xs -> (x:w) : wordsBy f (drop1 z)
        where (w,z) = break f xs

-- grouped 3 [1..8] = [[1, 2, 3], [4, 5, 6], [7, 8]]
grouped :: Int -> [a] -> [[a]]
grouped _ [] = []
grouped n l = xs : grouped n ys where (xs, ys) = splitAt n l 

grouped2 :: [a] -> [(a, a)]
grouped2 (x:y:xs) = (x, y) : grouped2 xs
grouped2 _ = [] 

-- sliding 3 [1..6] = [[1, 2, 3], [2, 3, 4], [3, 4, 5], [4, 5, 6]]
sliding :: Int -> [a] -> [[a]]
sliding n l = case (ys, l) of 
    (_, []) -> []
    ([], _) -> [xs]
    (_, _:zs) -> xs : sliding n zs
    where (xs, ys) = splitAt n l

pairwise :: (a -> a -> b) -> [a] -> [b]
pairwise f xs = [f y z | y : ys <- tails xs, z <- ys]

count :: Foldable t => (a -> Bool) -> t a -> Int
count f = foldl' (\acc x -> if f x then acc+1 else acc) 0
{-# INLINE count #-}

freqs :: Hashable a => [a] -> [(a, Int)]
freqs = HMap.toList . freqs'
{-# INLINE freqs #-}

freqs' :: Hashable a => [a] -> HashMap a Int
freqs' = HMap.fromListWith (+) . map (,1)
{-# INLINE freqs' #-}

mostCommon :: Hashable a => [a] -> Maybe a
mostCommon [] = Nothing
mostCommon xs = Just . fst . maximumOn snd $ freqs xs 

slice :: Int -> Int -> [a] -> [a]
slice start end = take (end - start + 1) . drop start
{-# INLINE slice #-}

average :: [Int] -> Double
average xs = realToFrac (sum xs) / genericLength xs

median :: Ord a => [a] -> a
median l = sort l !! (length l `div` 2)

flattenWithIndex :: [[a]] -> [(Int, Int, a)]
flattenWithIndex l =
    [(i, j, v)
    | (i, row) <- zip [0..] l
    , (j, v) <- zip [0..] row
    ]