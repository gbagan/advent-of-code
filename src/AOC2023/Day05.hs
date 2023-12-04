-- https://adventofcode.com/2023/day/5
module AOC2023.Day05 (solve) where
import           RIO
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (eol)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc)

parser :: Parser [[Int]]
parser = (decimal `sepEndBy1` eol) `sepEndBy1` eol

part1 :: [[Int]] -> Int
part1 _ = 1

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part1