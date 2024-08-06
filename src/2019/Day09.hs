module Day09 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, signedDecimal, sepBy1)
import           AOC.IntCode (runProgram)

parser :: Parser [Int]
parser = signedDecimal `sepBy1` ","

solve :: Text -> IO ()
solve = aoc parser (runProgram [1]) (runProgram [2])