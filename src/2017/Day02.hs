-- https://adventofcode.com/2017/day/2
module Day02 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, decimal, eol, hspace, sepBy1, sepEndBy1)
import           Data.List (delete, maximum, minimum)

parser :: Parser [[Int]]
parser = (decimal `sepBy1` hspace) `sepEndBy1` eol

part1, part2 :: [[Int]] -> Int
part1 rows = sum [maximum row - minimum row | row <- rows]
part2 rows = sum [q | row <- rows, x <- row, y <- delete x row, (q,0) <- [x `divMod` y]]

solve :: Text -> IO ()
solve = aoc parser part1 part2