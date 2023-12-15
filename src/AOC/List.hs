module AOC.List where

import           AOC.Prelude
import           Data.List (maximum, minimum)

allUnique :: Ord a => [a] -> Bool
allUnique xs = length (ordNub xs) == length xs

minimumMaybe :: Ord a => [a] -> Maybe a
minimumMaybe [] = Nothing
minimumMaybe l = Just $ minimum l

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

-- sliding 3 [1..6] = [[1, 2, 3], [2, 3, 4], [3, 4, 5], [4, 5, 6]]
sliding :: Int -> [a] -> [[a]]
sliding n l = case (ys, l) of 
    (_, []) -> []
    ([], _) -> [xs]
    (_, _:zs) -> xs : sliding n zs
    where (xs, ys) = splitAt n l

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f
{-# INLINE count #-}

slice :: Int -> Int -> [a] -> [a]
slice start end = take (end - start + 1) . drop start
{-# INLINE slice #-}

average :: [Int] -> Double
average xs = realToFrac (sum xs) / genericLength xs

flattenWithIndex :: [[a]] -> [(Int, Int, a)]
flattenWithIndex l =
    [(i, j, v)
    | (i, row) <- zip [0..] l
    , (j, v) <- zip [0..] row
    ]