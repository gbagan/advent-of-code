-- https://adventofcode.com/2015/day/2
module Day02 (solve) where
import           AOC.Prelude
import           Data.List (minimum)
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, decimal, eol)

data Box = Box !Int !Int !Int

parser :: Parser [Box]
parser = box `sepEndBy1` eol where
    box = Box <$> decimal <* "x" <*> decimal <* "x" <*> decimal

part1 :: [Box] -> Int
part1 = sum . map paper where
    paper (Box l w h) = 2 * sum areas + minimum areas where
        areas = [l*w, l*h, w*h]

part2 :: [Box] -> Int
part2 = sum . map ribbon where
    ribbon (Box l w h) = l * h *w + 2 * minimum [l+w, l+h, w+h]

solve :: Text -> IO ()
solve = aoc parser part1 part2