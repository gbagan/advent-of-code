-- https://adventofcode.com/2021/day/1
module Day01 (solve) where

import Text.Read (readMaybe)
import Util (count)

algo :: Int -> [Int] -> Int
algo n l = count id $ zipWith (<) l (drop n l)

solve :: String -> Maybe (Int, Int)
solve s = do
    xs <- traverse readMaybe (lines s)
    Just (algo 1 xs, algo 3 xs)
