-- https://adventofcode.com/2020/day/3
module AOC2020.Day05 (solve) where
import           RIO hiding (some)
import           RIO.List (sort)
import           RIO.List.Partial (maximum, tail)
import           Text.Megaparsec (sepEndBy1, some)
import           Text.Megaparsec.Char (char, eol)
import           Util (Parser, aoc, binToInt)

parser :: Parser [[Bool]]
parser = line `sepEndBy1` eol where
    line = some letter
    letter = False <$ char 'F' <|> True <$ char 'B' <|> False <$ char 'L' <|> True <$ char 'R'

part1 :: [[Bool]] -> Int
part1 = maximum . map binToInt

part2 :: [[Bool]] -> Maybe Int
part2 seats = listToMaybe [ x+1
                          | (x, y) <- zip seats' (tail seats')
                          , y-x == 2] where
    seats' = sort $ map binToInt seats

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2
