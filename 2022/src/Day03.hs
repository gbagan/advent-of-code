-- https://adventofcode.com/2022/day/3
module Day03 (solve) where
import           RIO hiding (some)
import           RIO.Char (isLower, ord)
import           RIO.List (intersect, splitAt)
import           RIO.List.Partial (head, foldl1')
import           Data.List.Split (chunksOf)
import           Text.Megaparsec (sepEndBy1, some)
import           Text.Megaparsec.Char (letterChar, eol)
import           Util (Parser, aoc)

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
part2 = sum . map (priority . head . foldl1' intersect) . chunksOf 3

solve :: Text -> RIO env ()
solve = aoc parser part1 part2
