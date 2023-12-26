-- https://adventofcode.com/2020/day/25
module Day25 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, eol, decimal)
import           AOC.Number (discreteLogarithm)

parser :: Parser (Int, Int)
parser = (,) <$> decimal <* eol <*> decimal

mul :: Int -> Int -> Int
mul x y = x * y `rem` 20201227

part1 :: (Int, Int) -> Int
part1 (cardPublicKey, doorPublicKey) = traceShow (discreteLogarithm 3 31 5) 0




solve :: Text -> IO ()
solve = aoc parser part1 (const (0 ::Int))