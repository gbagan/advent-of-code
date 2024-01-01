-- https://adventofcode.com/2016/day/20
module Day20 (solve) where
import           AOC.Prelude hiding (head, length)
import           Data.List (head)
import           AOC (aoc)
import           AOC.Parser (Parser, decimal, sepEndBy1, eol, format)
import           AOC.Range (Range(..), length, toDisjointUnion)

parser :: Parser [Range Int]
parser = range `sepEndBy1` eol where
    range = [format|$Range {decimal}-{decimal}|]

part1 :: [Range Int] -> Int
part1 = (+1) . _upper . head . toDisjointUnion

part2 :: [Range Int] -> Int
part2 ranges = 2^(32::Int) - sum (map length (toDisjointUnion ranges))

solve :: Text -> IO ()
solve = aoc parser part1 part2