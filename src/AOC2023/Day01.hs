-- https://adventofcode.com/2023/day/1
module AOC2023.Day01 (solve) where
import           RIO hiding (some)
import           RIO.Char (isDigit)
import           RIO.List (tails, isPrefixOf)
import           RIO.List.Partial (head, last)
import           RIO.Partial (read)
import           Text.Megaparsec (sepEndBy1, some)
import           Text.Megaparsec.Char (alphaNumChar, eol)
import           Util (Parser, aoc)

parser :: Parser [String]
parser = some alphaNumChar `sepEndBy1` eol

solveWith :: (String -> String) -> [String] -> Int
solveWith toDigits = sum . map lineToInt where
    lineToInt s = let x = toDigits s in read @Int [head x, last x]

toDigits1 :: String -> String
toDigits1 = filter isDigit

toDigits2 :: String -> String
toDigits2 = concatMap matchToken . tails where
    matchToken xs = [digit | (digit, token) <- tokens, token `isPrefixOf` xs]
    tokens = zip (['1'..'9'] ++ ['1'..'9']) ["1", "2", "3", "4", "5", "6", "7", "8", "9", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

solve :: MonadIO m => Text -> m ()
solve = aoc parser (solveWith toDigits1) (solveWith toDigits2)