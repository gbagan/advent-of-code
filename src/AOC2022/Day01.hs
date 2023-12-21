-- https://adventofcode.com/2022/day/1
module Day01 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, eol, decimal)

parser :: Parser [[Int]]
parser = (decimal `sepEndBy1` eol) `sepEndBy1` eol

solveFor :: Int -> [[Int]] -> Int
solveFor n = sum . take n . sortOn Down . map sum

solve :: Text -> IO ()
solve = aoc parser (solveFor 1) (solveFor 3)