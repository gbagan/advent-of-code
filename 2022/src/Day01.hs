-- https://adventofcode.com/2022/day/1
module Day01 (solve) where
import           RIO
import           RIO.List (sort)
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (eol)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc, takeEnd)

parser :: Parser [[Int]]
parser = (decimal `sepEndBy1` eol) `sepEndBy1` eol

solve' :: Int -> [[Int]] -> Int
solve' n = sum . takeEnd n . sort . map sum

solve :: MonadIO m => Text -> m ()
solve = aoc parser (solve' 1) (solve' 3)