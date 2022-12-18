-- https://adventofcode.com/2020/day/1
module AOC2020.Day01 (solve) where
import           RIO
import qualified Data.IntSet as Set
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (eol)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc)

parser :: Parser [Int]
parser = decimal `sepEndBy1` eol

part1 :: [Int] -> Maybe Int
part1 xs = listToMaybe [x * y | x <- xs, let y = 2020 - x, y `Set.member` s]
    where s = Set.fromList xs

part2 :: [Int] -> Maybe Int
part2 xs = listToMaybe [x * y * z| x <- xs, y <- xs, let z = 2020 - x - y, z `Set.member` s]
    where s = Set.fromList xs

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2