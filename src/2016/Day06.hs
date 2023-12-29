-- https://adventofcode.com/2016/day/6
module Day06 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, eol, lowerChar, sepEndBy1, some)
import           AOC.List (freqs, minimumOn, maximumOn)

parser :: Parser [String]
parser = some lowerChar `sepEndBy1` eol

mostFrequent, leastFrequent :: Hashable a => [a] -> a
mostFrequent = fst . maximumOn snd . freqs
leastFrequent = fst . minimumOn snd . freqs

part1, part2 :: [String] -> String
part1 = map mostFrequent . transpose
part2 = map leastFrequent . transpose

solve :: Text -> IO ()
solve = aoc parser part1 part2