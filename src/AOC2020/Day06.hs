-- https://adventofcode.com/2020/day/3
module Day06 (solve) where
import           AOC.Prelude hiding (group)
import           Data.List (foldl1')
import qualified Data.HashSet as Set
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, some, eol, lowerChar)

parser :: Parser [[[Char]]]
parser = group `sepEndBy1` eol where
    group = person `sepEndBy1` eol
    person = some lowerChar

part1 :: [[[Char]]] -> Int
part1 = sum . map (Set.size . Set.fromList . concat)

part2 :: [[[Char]]] -> Int
part2 = sum . map (Set.size . foldl1' Set.intersection . map Set.fromList)

solve :: Text -> IO ()
solve = aoc parser part1 part2
