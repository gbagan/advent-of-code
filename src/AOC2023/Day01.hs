-- https://adventofcode.com/2023/day/1
module AOC2023.Day01 (solve) where
import           RIO hiding (some)
import           RIO.List (tails, isPrefixOf)
import           RIO.List.Partial (head, last)
import           Text.Megaparsec (sepEndBy1, some)
import           Text.Megaparsec.Char (alphaNumChar, eol)
import           Util (Parser, aoc)

parser :: Parser [String]
parser = some alphaNumChar `sepEndBy1` eol

solveWith :: [(Int, String)] -> [String] -> Int
solveWith tokens = sum . map lineToInt where
    lineToInt s = let x = toDigits s in head x * 10 + last x
    toDigits = concatMap matchToken . tails
    matchToken xs = [digit | (digit, token) <- tokens, token `isPrefixOf` xs]

tokens1 :: [(Int, String)]
tokens1 = [(i, show i) | i <- [1..9]]

tokens2 :: [(Int, String)]
tokens2 = tokens1 ++ zip [1..9] ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

solve :: MonadIO m => Text -> m ()
solve = aoc parser (solveWith tokens1) (solveWith tokens2)