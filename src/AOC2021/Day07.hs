module Day07 (solve) where
import           AOC.Prelude
import           Data.List ((!!))
import           AOC (aoc)
import           AOC.Parser (Parser, char, decimal, sepBy1)
import           AOC.List (average)

parser :: Parser [Int]
parser = decimal `sepBy1` char ','

part1 :: [Int] -> Int
part1 xs = sum [abs (x - m) | x <- xs] where
        m = xs !! (length xs `div` 2)

part2 :: [Int] -> Int
part2 xs = sum [bin . abs $ x - m | x <- xs] where
        m = floor (average xs)
        bin n = n * (n + 1) `div` 2

solve :: Text -> IO ()
solve = aoc parser part1 part2