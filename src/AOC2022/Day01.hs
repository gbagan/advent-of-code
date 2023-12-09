-- https://adventofcode.com/2022/day/1
module AOC2022.Day01 (solve) where
import           AOC.Prelude
import           Data.List.Extra (takeEnd)
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, eol, decimal)

parser :: Parser [[Int]]
parser = (decimal `sepEndBy1` eol) `sepEndBy1` eol

solveWith :: Int -> [[Int]] -> Int
solveWith n = sum . takeEnd n . sort . map sum

solve :: Text -> IO ()
solve = aoc parser (solveWith 1) (solveWith 3)