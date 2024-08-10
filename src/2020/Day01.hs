-- https://adventofcode.com/2020/day/1
module Day01 (solve) where
import           AOC.Prelude
import qualified Data.IntSet as Set
import           AOC (aoc)
import           AOC.List (headMaybe)
import           AOC.Parser (Parser, sepEndBy1, eol, decimal)

parser :: Parser [Int]
parser = decimal `sepEndBy1` eol

part1 :: [Int] -> Maybe Int
part1 xs = headMaybe [x * y | x <- xs, let y = 2020 - x, y `Set.member` s]
    where s = Set.fromList xs

part2 :: [Int] -> Maybe Int
part2 xs = headMaybe [x * y * z| x <- xs, y <- xs, let z = 2020 - x - y, z `Set.member` s]
    where s = Set.fromList xs

solve :: Text -> IO ()
solve = aoc parser part1 part2