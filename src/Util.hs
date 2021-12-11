module Util where
import Data.Char (digitToInt, isDigit)
import Data.List (sort, genericLength)
import Data.Map (Map)
import qualified Data.Map as Map

type Point = (Int, Int)

adjacentPoints :: Point -> [Point]
adjacentPoints (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

kingAdjacentPoints :: Point -> [Point]
kingAdjacentPoints (x, y) = adjacentPoints (x, y) ++ [(x-1, y-1), (x+1, y-1), (x-1, y+1), (x+1, y+1)]

digitToIntMaybe :: Char -> Maybe Int
digitToIntMaybe c | isDigit c = Just $ digitToInt c
                  | otherwise = Nothing

freqs :: Ord a => [a] -> Map a Int
freqs = Map.fromListWith (+) . map (,1)

average :: [Int] -> Double
average xs = realToFrac (sum xs) / genericLength xs

majority :: (a -> Bool) -> [a] -> Bool
majority f l = 2*m >= n where
                m = length $ filter f l
                n = length l

median :: Ord a => [a] -> a
median l = sort l !! (length l `div` 2)

parse2dMap :: (Char -> Maybe a) -> String -> Maybe (Map Point a)
parse2dMap parseChar str = do
    l <- traverse (traverse parseChar) (lines str)
    Just $ Map.fromList
        [((i, j), v) 
        | (j, row) <- zip [0..] l
        , (i, v) <- zip [0..] row
        ]