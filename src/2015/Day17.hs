-- https://adventofcode.com/2015/day/17
module Day17 (solve) where
import           AOC.Prelude hiding (min)
import           AOC (aoc)
import           AOC.Parser (Parser, decimal, eol, sepEndBy1)
import           Data.List (minimum)
import           AOC.List (count)

parser :: Parser [Int]
parser = decimal `sepEndBy1` eol

subsetSizes :: Int -> [Int] -> [Int]
subsetSizes n [] = [0 | n == 0]
subsetSizes n (x:xs) | n < 0 = []
                      | otherwise = subsetSizes n xs ++ map (+1) (subsetSizes (n-x) xs)

part1 :: [Int] -> Int
part1 = length . subsetSizes 150

part2 :: [Int] -> Int
part2 xs = count (==min) sizes where
    sizes = subsetSizes 150 xs
    min = minimum sizes


solve :: Text -> IO ()
solve = aoc parser part1 part2
