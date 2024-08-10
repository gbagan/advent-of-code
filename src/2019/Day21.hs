-- https://adventofcode.com/2019/day/21
module Day21 (solve) where
import           AOC.Prelude hiding (unlines)
import           AOC (aoc)
import           AOC.Parser (Parser, signedDecimal, sepBy1)
import           AOC.IntCode (runProgram)
import           AOC.List (lastMaybe)
import           Data.List (unlines)

parser :: Parser [Int]
parser = signedDecimal `sepBy1` ","

code1, code2 :: [String]
code1 = ["NOT C J", "AND D J", "NOT A T", "OR T J", "WALK"]
code2 = ["OR A T", "AND B T", "AND C T", "NOT T J", "OR E T", "OR H T", "AND T J", "AND D J", "RUN"]

solveWith :: [String] -> [Int] -> Maybe Int
solveWith code pgm = lastMaybe $ runProgram (map ord (unlines code)) pgm

solve :: Text -> IO ()
solve = aoc parser (solveWith code1) (solveWith code2)