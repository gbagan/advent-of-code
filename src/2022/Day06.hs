-- https://adventofcode.com/2022/day/6
module Day06 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, anySingle, some)
import           AOC.List (allDistinct, sliding)

parser :: Parser String
parser = some anySingle

solveFor :: Int -> String -> Maybe Int
solveFor n = fmap (+n) . findIndex allDistinct . sliding n

solve :: Text -> IO ()
solve = aoc parser (solveFor 4) (solveFor 14)
