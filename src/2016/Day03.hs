-- https://adventofcode.com/2016/day/3
module Day03 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, decimal, eol, hspace, sepEndBy1)
import           AOC.List (count)

parser :: Parser [(Int, Int, Int)]
parser = row `sepEndBy1` eol where
    row = (,,) <$> (hspace *> decimal) <* hspace <*> decimal <* hspace <*> decimal

isTriangle :: (Int, Int, Int) -> Bool
isTriangle (x, y, z) = x + y > z && x + z > y && y + z > x

transposeTriples :: [(Int, Int, Int)] -> [(Int, Int, Int)]
transposeTriples ((x1, y1, z1) : (x2, y2, z2) : (x3, y3, z3) : xs) =
    (x1, x2, x3) : (y1, y2, y3) : (z1, z2, z3) : transposeTriples xs
transposeTriples _ = []

part1, part2 :: [(Int, Int, Int)] -> Int
part1 = count isTriangle
part2 = count isTriangle . transposeTriples

solve :: Text -> IO ()
solve = aoc parser part1 part2