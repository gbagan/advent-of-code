-- https://adventofcode.com/2019/day/5
module Day05 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, signedDecimal, sepEndBy1)
import qualified Data.Vector.Unboxed as V
import           AOC.IntCode (runProgram)

parser :: Parser (V.Vector Int)
parser = V.fromList <$> signedDecimal `sepEndBy1` ","

solveWith :: Int -> V.Vector Int -> Maybe Int
solveWith input pgm = listToMaybe output where
    (_, output) = runProgram [input] pgm

solve :: Text -> IO ()
solve = aoc parser (solveWith 1) (solveWith 5)