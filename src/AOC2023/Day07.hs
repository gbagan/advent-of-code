-- https://adventofcode.com/2023/day/7
module AOC2023.Day07 (solve) where
import           RIO
import           RIO.List (sort)
import           Data.List.Extra (takeEnd)
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (eol)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc)

parser :: Parser [[Int]]
parser = (decimal `sepEndBy1` eol) `sepEndBy1` eol

solveWith :: Int -> [[Int]] -> Int
solveWith n = sum . takeEnd n . sort . map sum

solve :: MonadIO m => Text -> m ()
solve = aoc parser (solveWith 1) (solveWith 3)