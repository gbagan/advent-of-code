-- https://adventofcode.com/2023/day/1
module Day01 (solve) where
import           AOC.Prelude hiding (head, last)
import           Data.List (head, last)
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, some, alphaNumChar, eol)

type Token = (Int, String)

parser :: Parser [String]
parser = some alphaNumChar `sepEndBy1` eol

solveFor :: [Token] -> [String] -> Int
solveFor tokens = sum . map lineToInt where
    lineToInt s = let x = toDigits s in head x * 10 + last x
    toDigits = concatMap matchToken . tails
    matchToken xs = [digit | (digit, token) <- tokens, token `isPrefixOf` xs]

tokens1 :: [Token]
tokens1 = [(i, show i) | i <- [1..9]]

tokens2 :: [Token]
tokens2 = tokens1 ++ zip [1..9] ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

solve :: Text -> IO ()
solve = aoc parser (solveFor tokens1) (solveFor tokens2)