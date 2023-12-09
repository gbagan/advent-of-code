-- https://adventofcode.com/2021/day/1
module AOC2021.Day01 (solve) where
import           Relude
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (eol)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc, count)

parser :: Parser [Int]
parser = decimal `sepEndBy1` eol

algo :: Int -> [Int] -> Int
algo n l = count id $ zipWith (<) l (drop n l)

solve :: MonadIO m => Text -> m ()
solve = aoc parser (algo 1) (algo 3)

