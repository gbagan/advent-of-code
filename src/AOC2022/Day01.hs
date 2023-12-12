-- https://adventofcode.com/2022/day/1
module AOC2022.Day01 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, eol, decimal)
import           AOC.Util (takeEnd)

parser :: Parser [[Int]]
parser = (decimal `sepEndBy1` eol) `sepEndBy1` eol

solveFor :: Int -> [[Int]] -> Int
solveFor n = sum . takeEnd n . sort . map sum

solve :: Text -> IO ()
solve = aoc parser (solveFor 1) (solveFor 3)