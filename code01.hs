-- https://adventofcode.com/2021/day/1
module Main where

countAug :: Int -> [Int] -> Int
countAug n l = length . filter id $ zipWith (<) l (drop n l)

main = do
    xs <- map read . lines <$> readFile "data01"
    print $ countAug 1 xs
    print $ countAug 3 xs
