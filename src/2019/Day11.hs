module Day11 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, signedDecimal, sepEndBy1)
import           AOC.IntCode (runProgram)

parser :: Parser [Int]
parser = signedDecimal `sepEndBy1` ","

part1 :: [Int] -> Int
part1 _ = 0

part2 :: [Int] -> Int
part2 _ = 0

solve :: Text -> IO ()
solve = aoc parser part1 part2