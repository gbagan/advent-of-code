-- https://adventofcode.com/2019/day/19
module Day19 (solve) where
import           AOC.Prelude hiding (last)
import           AOC (aoc)
import           AOC.Parser (Parser, signedDecimal, sepBy1)
import           AOC.IntCode (runMachine, newMachine)
import           AOC.List (count, findJust)

parser :: Parser [Int]
parser = signedDecimal `sepBy1` ","

part1 :: [Int] -> Int
part1 pgm = count (==[1]) [runMachine inp machine | inp <- input] where
    input = [[i, j] | i <- [0..49], j <- [0..49]]
    machine = newMachine pgm

part2 :: [Int] -> Int
part2 pgm = go 0 100 where
    machine = newMachine pgm
    test x y = runMachine [x, y] machine == [1]
    go x y | test (x'+99) (y-99) = x' * 10_000 + y - 99
           | otherwise           = go x' (y+1)
        where
        x' = findJust (`test` y) [x..]

solve :: Text -> IO ()
solve = aoc parser part1 part2