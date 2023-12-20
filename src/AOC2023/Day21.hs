-- https://adventofcode.com/2023/day/21
module AOC2023.Day21 (solve) where
import           AOC.Prelude hiding (head, last)
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, some, alphaNumChar, eol)

parser :: Parser [String]
parser = some alphaNumChar `sepEndBy1` eol

part1 :: [String] -> Int
part1 _ = 0 

part2 :: [String] -> Int
part2 _ = 0 

solve :: Text -> IO ()
solve = aoc parser part1 part2