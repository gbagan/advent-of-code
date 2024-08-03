-- https://adventofcode.com/2019/day/7
module Day07 (solve) where
import           AOC.Prelude hiding (head, last)
import           AOC (aoc)
import           AOC.Parser (Parser, signedDecimal, sepEndBy1)
import           AOC.IntCode (runProgram)
import           Data.List (head, last, maximum, foldl)

parser :: Parser [Int]
parser = signedDecimal `sepEndBy1` ","

run :: [Int] -> [Int] -> [Int]
run phases pgm = output where
    output = foldl go (0:output) phases
    go prevOutput phase = runProgram (phase:prevOutput) pgm

part1 :: [Int] -> Int
part1 pgm = maximum [head (run phases pgm) | phases <- permutations [0..4]]

part2 :: [Int] -> Int
part2 pgm = maximum [last (run phases pgm) | phases <- permutations [5..9]]

solve :: Text -> IO ()
solve = aoc parser part1 part2