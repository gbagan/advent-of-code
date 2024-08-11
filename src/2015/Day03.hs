-- https://adventofcode.com/2015/day/3
module Day03 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, choice, some)
import           AOC.List (grouped)
import           AOC.V2 (V2, origin, north, south, west, east)

parser :: Parser [V2 Int]
parser = some direction where
    direction = choice [ north <$ "^"
                       , south <$ "v"
                       , west  <$ "<"
                       , east  <$ ">"
                       ]

part1 :: [V2 Int] -> Int
part1 = length . ordNub . scanl' (+) origin

part2 :: [V2 Int] -> Int
part2 = length . ordNub . concatMap (scanl' (+) origin) . transpose . grouped 2

solve :: Text -> IO ()
solve = aoc parser part1 part2