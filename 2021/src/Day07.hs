module Day07 (solve) where
import           RIO
import           RIO.List.Partial ((!!))
import           Text.Megaparsec (sepBy1)
import           Text.Megaparsec.Char (char)
import           Text.Megaparsec.Char.Lexer (decimal)
import Util (Parser, aoc, average)

parser :: Parser [Int]
parser = decimal `sepBy1` char ','

part1 :: [Int] -> Int
part1 xs = sum [abs (x - m) | x <- xs] where
        m = xs !! (length xs `div` 2)

part2 :: [Int] -> Int
part2 xs = sum [bin . abs $ x - m | x <- xs] where
        m = floor (average xs)
        bin n = n * (n + 1) `div` 2

solve :: (HasLogFunc env) => Text -> RIO env ()
solve = aoc parser part1 part2