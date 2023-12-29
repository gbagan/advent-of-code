-- https://adventofcode.com/2016/day/1
module Day01 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, eol, some, lowerChar, sepEndBy1)

parser :: Parser [String]
parser = some lowerChar `sepEndBy1` eol

part1 :: [String] -> Int
part1 _ = 0

part2 :: [String] -> Int
part2 _ = 0

solve :: Text -> IO ()
solve = aoc parser part1 part2
