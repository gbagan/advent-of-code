-- https://adventofcode.com/2019/day/5
module Day05 (solve) where
import           AOC.Prelude hiding (last)
import           AOC (aoc)
import           AOC.Parser (Parser, signedDecimal, sepBy1)
import           AOC.IntCode (runProgram)
import           Data.List (last)

parser :: Parser [Int]
parser = signedDecimal `sepBy1` ","

solveWith :: Int -> [Int] -> Int
solveWith input = last . runProgram [input]

solve :: Text -> IO ()
solve = aoc parser (solveWith 1) (solveWith 5)