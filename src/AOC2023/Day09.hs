-- https://adventofcode.com/2023/day/9
module AOC2023.Day09 (solve) where
import           AOC.Prelude hiding (last, tail)
import           Relude.Unsafe (last, tail)
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, eol, hspace, signedDecimal)

parser :: Parser [[Int]]
parser = (signedDecimal `sepEndBy1` hspace) `sepEndBy1` eol

solveWith :: ([Int] -> [Int]) -> [[Int]] -> Int
solveWith f = sum . map (extrapolate . f) where
    extrapolate = sum . map last . takeWhile (any (/=0)) . iterate' diff
    diff l = zipWith (-) (tail l) l

solve :: Text -> IO ()
solve = aoc parser (solveWith id) (solveWith reverse)