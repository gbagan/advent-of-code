module Day07 (solve) where
import Data.List (sort)
import Text.Read (readMaybe)
import           Text.Megaparsec (sepBy1)
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Util (Parser, aocTemplate, average)

parser :: Parser [Int]
parser = L.decimal `sepBy1` P.char ','

part1 :: [Int] -> Int
part1 xs = sum [abs (x - m) | x <- xs] where
        m = xs !! (length xs `div` 2)

part2 :: [Int] -> Int
part2 xs = sum [bin . abs $ x - m | x <- xs] where
        m = floor (average xs)
        bin n = n * (n + 1) `div` 2

solve :: String -> IO ()
solve = aocTemplate parser (Just . part1) (Just . part2)