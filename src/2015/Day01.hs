-- https://adventofcode.com/2015/day/1
module Day01 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, some)

parser :: Parser [Int]
parser = some $ 1 <$ "(" <|> (-1) <$ ")"

part1 :: [Int] -> Int
part1 = sum

part2 :: [Int] -> Maybe Int
part2 = findIndex (<0) . scanl' (+) 0

solve :: Text -> IO ()
solve = aoc parser part1 part2
