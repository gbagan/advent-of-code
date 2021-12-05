-- https://adventofcode.com/2021/day/1
module Day01 (solve) where

algo :: Int -> [Int] -> Int
algo n l = length . filter id $ zipWith (<) l (drop n l)

solve :: String -> Maybe (Int, Int)
solve s =
    let xs = map read . lines $ s in
    Just (algo 1 xs, algo 3 xs)
