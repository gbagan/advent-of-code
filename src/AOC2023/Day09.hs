-- https://adventofcode.com/2023/day/9
module AOC2023.Day09 (solve) where
import           RIO hiding (some)
import           RIO.List (iterate)
import           RIO.List.Partial (head, last, tail)
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (eol, hspace)
import           Util (Parser, aoc)
import           Util.Parser (signedDecimal)

parser :: Parser [[Int]]
parser = (signedDecimal `sepEndBy1` hspace) `sepEndBy1` eol

solveWith :: (Int -> [Int] -> Int) -> [[Int]] -> Int
solveWith f = sum . map extrapolate where
    extrapolate = findValue . reverse . takeWhile (any (/=0)) . iterate diff
    findValue = foldl' f 0
    diff l = zipWith (-) (tail l) l

part1 :: [[Int]] -> Int
part1 = solveWith \acc xs -> acc + last xs

part2 :: [[Int]] -> Int
part2 = solveWith \acc xs -> head xs - acc 

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2