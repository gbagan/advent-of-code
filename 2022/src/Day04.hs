-- https://adventofcode.com/2022/day/4
module Day04 (solve) where
import           RIO
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (char, eol)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc, count)

parser :: Parser [((Int, Int), (Int, Int))]
parser = pair `sepEndBy1` eol where
    pair = (,) <$> assignment <* char ',' <*> assignment
    assignment = (,) <$> decimal <* char '-' <*> decimal 

part1 :: [((Int, Int), (Int, Int))] -> Int
part1 = count fullyContains where
    fullyContains ((x1, y1), (x2, y2)) = x1 <= x2 && y2 <= y1 || x2 <= x1 && y1 <= y2

part2 :: [((Int, Int), (Int, Int))] -> Int
part2 = count overlaps where
    overlaps ((x1, y1), (x2, y2)) = max x1 x2 <= min y1 y2

solve :: (HasLogFunc env) => Text -> RIO env ()
solve = aoc parser part1 part2
