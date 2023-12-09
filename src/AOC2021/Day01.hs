-- https://adventofcode.com/2021/day/1
module AOC2021.Day01 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, decimal, eol, sepEndBy1)
import           AOC.Util (count)

parser :: Parser [Int]
parser = decimal `sepEndBy1` eol

algo :: Int -> [Int] -> Int
algo n l = count id $ zipWith (<) l (drop n l)

solve :: Text -> IO ()
solve = aoc parser (algo 1) (algo 3)

