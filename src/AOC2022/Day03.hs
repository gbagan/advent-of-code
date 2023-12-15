-- https://adventofcode.com/2022/day/3
module AOC2022.Day03 (solve) where
import           AOC.Prelude hiding (head)
import           Data.Char (isLower)
import           Data.List (head, foldl1', intersect)
import           AOC (aoc)
import           AOC.List (grouped)
import           AOC.Parser (Parser, sepEndBy1, some, eol, letterChar)

parser :: Parser [String]
parser = some letterChar `sepEndBy1` eol

priority :: Char -> Int
priority c | isLower c = ord c - ord 'a' + 1
           | otherwise = ord c - ord 'A' + 27

part1 :: [String] -> Int
part1 = sum . map findTheSame where
    findTheSame = priority . head . uncurry intersect . splitAtMiddle
    splitAtMiddle s = splitAt (length s `div` 2) s

part2 :: [String] -> Int
part2 = sum . map (priority . head . foldl1' intersect) . grouped 3

solve :: Text -> IO ()
solve = aoc parser part1 part2
