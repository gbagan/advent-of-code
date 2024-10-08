-- https://adventofcode.com/2020/day/3
module Day05 (solve) where
import           AOC.Prelude
import           Data.List (maximum)
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, some, char, eol)
import           AOC.List (headMaybe)
import           AOC.Util (binToInt)

parser :: Parser [[Bool]]
parser = line `sepEndBy1` eol where
    line = some letter
    letter = False <$ char 'F' <|> True <$ char 'B' <|> False <$ char 'L' <|> True <$ char 'R'

part1 :: [[Bool]] -> Int
part1 = maximum . map binToInt

part2 :: [[Bool]] -> Maybe Int
part2 seats = headMaybe [ x+1
                        | (x, y) <- zip seats' (drop 1 seats')
                        , y-x == 2] where
    seats' = sort $ map binToInt seats

solve :: Text -> IO ()
solve = aoc parser part1 part2
