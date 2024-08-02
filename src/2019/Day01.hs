-- https://adventofcode.com/2019/day/1
module Day01 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, decimal, sepEndBy1, eol)

parser :: Parser [Int]
parser = decimal `sepEndBy1` eol

part1 :: [Int] -> Int
part1 = sum . map \n -> n `div` 3 - 2

part2 :: [Int] -> Int
part2 = sum . map f where
    f n | n <= 5    = 0
        | otherwise = m + f m where m = n `div` 3 - 2

solve :: Text -> IO ()
solve = aoc parser part1 part2
