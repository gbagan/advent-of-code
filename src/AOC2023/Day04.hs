-- https://adventofcode.com/2023/day/4
module AOC2023.Day04 (solve) where
import           RIO
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (eol)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc)

parser :: Parser [Int]
parser = decimal `sepEndBy1` eol

part1 :: [Int] -> Int
part1 _ = 0

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part1