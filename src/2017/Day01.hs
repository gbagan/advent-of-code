-- https://adventofcode.com/2017/day/1
module Day01 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, digitChar, some)
import           Data.Char (digitToInt)

parser :: Parser [Int]
parser = some $ digitToInt <$> digitChar

solveFor :: Int -> [Int] -> Int
solveFor n xs = sum $ zipWith (\x y -> if x == y then x else 0) xs (drop n xs ++ xs)

part1, part2 :: [Int] -> Int
part1 = solveFor 1
part2 xs = solveFor (length xs `div` 2) xs

solve :: Text -> IO ()
solve = aoc parser part1 part2
