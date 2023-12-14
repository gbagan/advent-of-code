-- https://adventofcode.com/2021/day/1
module AOC2021.Day01 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, decimal, eol, sepEndBy1)
import           AOC.List (count)

parser :: Parser [Int]
parser = decimal `sepEndBy1` eol

solveFor :: Int -> [Int] -> Int
solveFor n l = count id $ zipWith (<) l (drop n l)

solve :: Text -> IO ()
solve = aoc parser (solveFor 1) (solveFor 3)

