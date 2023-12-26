-- https://adventofcode.com/2015/day/10
module Day10 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           Data.Char (digitToInt)
import           AOC.Parser (Parser, some, digitChar)
import           AOC.Util (times)

parser :: Parser [Int]
parser = some (digitToInt <$> digitChar)

step :: [Int] -> [Int]
step [] = []
step (x:xs) = 1 + length ys : x : step zs where
    (ys, zs) = span (==x) xs

solveFor :: Int -> [Int] -> Int
solveFor n = length . times n step

solve :: Text -> IO ()
solve = aoc parser (solveFor 40) (solveFor 50)