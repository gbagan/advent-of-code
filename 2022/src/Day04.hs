-- https://adventofcode.com/2022/day/4
module Day04 (solve) where
import           RIO
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (char, eol)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc, count)

data Interval = Interval !Int !Int

parser :: Parser [(Interval, Interval)]
parser = pair `sepEndBy1` eol where
    pair = (,) <$> assignment <* char ',' <*> assignment
    assignment = Interval <$> decimal <* char '-' <*> decimal 

part1 :: [(Interval, Interval)] -> Int
part1 = count fullyContains where
    fullyContains (Interval x1 y1, Interval x2 y2) = x1 <= x2 && y2 <= y1 || x2 <= x1 && y1 <= y2

part2 :: [(Interval, Interval)] -> Int
part2 = count overlaps where
    overlaps (Interval x1 y1, Interval x2 y2) = max x1 x2 <= min y1 y2

solve :: (HasLogFunc env) => Text -> RIO env ()
solve = aoc parser part1 part2
