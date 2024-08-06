-- https://adventofcode.com/2017/day/4
module Day04 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, eol, hspace, lowerChar, sepBy1, sepEndBy1, some)
import           AOC.List (allDistinct, count)

parser :: Parser [[String]]
parser = (some lowerChar `sepBy1` hspace) `sepEndBy1` eol

solveFor :: (String -> String) -> [[String]] -> Int
solveFor f = count (allDistinct . map f)

solve :: Text -> IO ()
solve = aoc parser (solveFor id) (solveFor sort)
