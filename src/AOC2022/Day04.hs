-- https://adventofcode.com/2022/day/4
module Day04 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, eol, decimal)
import           AOC.List (count)
import           AOC.Interval (Interval(..), isSubsetOf, overlaps)

type Input = [(Interval Int, Interval Int)]

parser :: Parser Input
parser = pair `sepEndBy1` eol where
    pair = (,) <$> assignment <* "," <*> assignment
    assignment = Interval <$> decimal <* "-" <*> decimal 

part1 :: Input -> Int
part1 = count fullyContains where
    fullyContains (itv1, itv2) = itv1 `isSubsetOf` itv2 || itv2 `isSubsetOf` itv1

part2 :: Input -> Int
part2 = count (uncurry overlaps)

solve :: Text -> IO ()
solve = aoc parser part1 part2
