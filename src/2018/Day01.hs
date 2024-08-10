-- https://adventofcode.com/2018/day/1
module Day01 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.List (findDuplicate)
import           AOC.Parser (Parser, sepEndBy1, eol, decimal)

parser :: Parser [Int]
parser = signedDecimal `sepEndBy1` eol where
    signedDecimal = (id <$ "+" <|> negate <$ "-") <*> decimal

part1 :: [Int] -> Int
part1 = sum

part2 :: [Int] -> Maybe Int
part2 = findDuplicate . scanl' (+) 0 . cycle

solve :: Text -> IO ()
solve = aoc parser part1 part2