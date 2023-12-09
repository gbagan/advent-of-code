-- https://adventofcode.com/2023/day/9
module AOC2023.Day09 (solve) where
import           Relude hiding (some, last, tail)
import           Relude.Unsafe (last, tail)
import           Util (Parser, aoc)
import           Util.Parser (sepEndBy1, eol, hspace, signedDecimal)

parser :: Parser [[Int]]
parser = (signedDecimal `sepEndBy1` hspace) `sepEndBy1` eol

solveWith :: ([Int] -> [Int]) -> [[Int]] -> Int
solveWith f = sum . map (extrapolate . f) where
    extrapolate = sum . map last . takeWhile (any (/=0)) . iterate diff
    diff l = zipWith (-) (tail l) l

solve :: MonadIO m => Text -> m ()
solve = aoc parser (solveWith id) (solveWith reverse)