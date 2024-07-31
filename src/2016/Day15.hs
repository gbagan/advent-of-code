-- https://adventofcode.com/2016/day/15
module Day15 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, decimal, eol, sepEndBy1, scanf)
import           AOC.Number (chineseRemainder)

type Input = [(Integer, Integer, Integer, Integer)]

parser :: Parser Input
parser = row `sepEndBy1` eol where
    row = [scanf|Disc #{decimal} has {decimal} positions; at time={decimal}, it is at position {decimal}.|]

part1, part2 :: Input -> Maybe Integer
part1 discs = fst <$> chineseRemainder [(t-p-i, m) | (i, m, t, p) <- discs]
part2 discs = part1 ((genericLength discs + 1, 11, 0, 0) : discs)

solve :: Text -> IO ()
solve = aoc parser part1 part2