-- https://adventofcode.com/2023/day/11
module AOC2023.Day11 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, decimal, eol, sepEndBy1)

parser :: Parser [Int]
parser = decimal `sepEndBy1` eol

part1 :: [Int] -> Int
part1 _ = 0

part2 :: [Int] -> Int
part2 _ = 0

solve :: Text -> IO ()
solve = aoc parser part1 part2
