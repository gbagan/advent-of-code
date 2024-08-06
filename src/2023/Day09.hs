-- https://adventofcode.com/2023/day/9
module Day09 (solve) where
import           AOC.Prelude hiding (last, tail)
import           Data.List (last)
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, eol, hspace, signedDecimal)

parser :: Parser [[Int]]
parser = (signedDecimal `sepEndBy1` hspace) `sepEndBy1` eol

solveFor :: ([Int] -> [Int]) -> [[Int]] -> Int
solveFor f = sum . map (extrapolate . f) where
    extrapolate = sum . map last . takeWhile (any (/=0)) . iterate' differentiate
    differentiate l = zipWith (-) (drop 1 l) l

solve :: Text -> IO ()
solve = aoc parser (solveFor id) (solveFor reverse)