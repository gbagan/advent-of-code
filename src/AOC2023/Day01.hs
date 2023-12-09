-- https://adventofcode.com/2023/day/1
module AOC2023.Day01 (solve) where
import           Relude hiding (some, head, last)
import           Relude.Unsafe (head, last)
import           Util (aoc)
import           Util.Parser (Parser, sepEndBy1, some, alphaNumChar, eol)

type Token = (Int, String)

parser :: Parser [String]
parser = some alphaNumChar `sepEndBy1` eol

solveWith :: [Token] -> [String] -> Int
solveWith tokens = sum . map lineToInt where
    lineToInt s = let x = toDigits s in head x * 10 + last x
    toDigits = concatMap matchToken . tails
    matchToken xs = [digit | (digit, token) <- tokens, token `isPrefixOf` xs]

tokens1 :: [Token]
tokens1 = [(i, show i) | i <- [1..9]]

tokens2 :: [Token]
tokens2 = tokens1 ++ zip [1..9] ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

solve :: MonadIO m => Text -> m ()
solve = aoc parser (solveWith tokens1) (solveWith tokens2)