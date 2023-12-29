-- https://adventofcode.com/2015/day/25
module Day25 (solve) where
import           AOC.Prelude hiding (mod, exp)
import           AOC (aoc)
import           AOC.Parser (Parser, format, decimal)
import           AOC.Number (power)

parser :: Parser (Int, Int)
parser = [format| To continue, please consult the code grid in the manual.  Enter the code at row {decimal}, column {decimal}.|]

part1 :: (Int, Int) -> Int
part1 (row, col) = firstCode `mul` power mul base exp where
    firstCode = 20151125
    base = 252533
    mod = 33554393
    exp = (row + col - 1) * (row + col - 2) `quot` 2 + col - 1
    mul x y = x * y `rem` mod

solve :: Text -> IO ()
solve = aoc parser part1 (const (0::Int))
