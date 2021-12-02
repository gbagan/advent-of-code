-- https://adventofcode.com/2021/day/1
module Main where

algo :: Int -> [Int] -> Int
algo n l = length . filter id $ zipWith (<) l (drop n l)

main = do
    xs <- map read . lines <$> readFile "data01"
    print $ algo 1 xs
    print $ algo 3 xs
