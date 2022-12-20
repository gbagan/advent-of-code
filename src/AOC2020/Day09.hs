-- https://adventofcode.com/2020/day/9
module AOC2020.Day09 (solve) where
import           RIO
import           RIO.List (find)
import qualified Data.IntSet as Set
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (eol)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Util (Parser, aoc)
import           Data.List.Split (divvy)

parser :: Parser [Int]
parser = decimal `sepEndBy1` eol

part1 :: [Int] -> Maybe Int
part1 = listToMaybe <=< (find test . divvy 26 1 . reverse) where
    test [] = False
    test (x:xs) = xs & all \y -> y+y == x || (x - y) `Set.notMember` s where
        s = Set.fromList xs

part2 :: [Int] -> Maybe Int
part2 xs = listToMaybe [x * y * z| x <- xs, y <- xs, let z = 2020 - x - y, z `Set.member` s]
    where s = Set.fromList xs

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2