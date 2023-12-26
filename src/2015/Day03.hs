-- https://adventofcode.com/2015/day/3
module Day03 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, choice, some)
import           AOC.List (grouped)
import           AOC.V2 (V2(..))

parser :: Parser [V2 Int]
parser = some direction where
    direction = choice [ V2 1 0    <$ "^"
                       , V2 (-1) 0 <$ "v"
                       , V2 0 (-1) <$ "<"
                       , V2 0 1    <$ ">"
                       ]

part1 :: [V2 Int] -> Int
part1 = length . ordNub . scanl' (+) (V2 0 0)

part2 :: [V2 Int] -> Int
part2 = length . ordNub . concatMap (scanl' (+) (V2 0 0)) . transpose . grouped 2

solve :: Text -> IO ()
solve = aoc parser part1 part2