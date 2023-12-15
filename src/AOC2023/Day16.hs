-- https://adventofcode.com/2023/day/16
module AOC2023.Day16 (solve) where
import           AOC.Prelude
import qualified Data.IntSet as Set
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, eol, decimal)

parser :: Parser [Int]
parser = decimal `sepEndBy1` eol

part1 :: [Int] -> Int
part1 _ = 0

part2 :: [Int] -> Int
part2 _ = 0

solve :: Text -> IO ()
solve = aoc parser part1 part2