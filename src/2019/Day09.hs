module Day09 (solve) where
import           AOC.Prelude hiding (head, last)
import           AOC (aoc)
import           AOC.Parser (Parser, signedDecimal, sepEndBy1)
import           AOC.IntCode (runProgram)

parser :: Parser [Int]
parser = signedDecimal `sepEndBy1` ","

solve :: Text -> IO ()
solve = aoc parser (runProgram [1]) (runProgram [2])