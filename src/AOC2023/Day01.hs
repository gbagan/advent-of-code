-- https://adventofcode.com/2023/day/1
module AOC2023.Day01 (solve) where
import           RIO hiding (some)
import           RIO.Char (isDigit)
import           RIO.List (isPrefixOf)
import           RIO.List.Partial (head, last)
import           RIO.Partial (read)
import           Text.Megaparsec (sepEndBy1, some)
import           Text.Megaparsec.Char (alphaNumChar, eol)
import           Util (Parser, aoc)

parser :: Parser [String]
parser = some alphaNumChar `sepEndBy1` eol

solveWith :: (String -> String) -> [String] -> Int
solveWith f = sum . map readInt where
    readInt s = let x = f s in read @Int [head x, last x]

filter1 :: String -> String
filter1 = filter isDigit

filter2 :: String -> String
filter2 "" = ""
filter2 (x:xs) | "one" `isPrefixOf` (x:xs) = '1' : filter2 xs
               | "two" `isPrefixOf` (x:xs) = '2' : filter2 xs
               | "three" `isPrefixOf` (x:xs) = '3' : filter2 xs
               | "four" `isPrefixOf` (x:xs) = '4' : filter2 xs
               | "five" `isPrefixOf` (x:xs) = '5' : filter2 xs
               | "six" `isPrefixOf` (x:xs) = '6' : filter2 xs
               | "seven" `isPrefixOf` (x:xs) = '7' : filter2 xs
               | "eight" `isPrefixOf` (x:xs) = '8' : filter2 xs
               | "nine" `isPrefixOf` (x:xs) = '9' : filter2 xs
               | isDigit x = x : filter2 xs
               | otherwise = filter2 xs

solve :: MonadIO m => Text -> m ()
solve = aoc parser (solveWith filter1) (solveWith filter2)