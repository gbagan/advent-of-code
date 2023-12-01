-- https://adventofcode.com/2023/day/1
module AOC2023.Day01 (solve) where
import           RIO hiding (some)
import           RIO.Char (isDigit)
import           RIO.List (find, isPrefixOf)
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
filter2 (x:xs) | isDigit x = x : filter2 xs
               | otherwise =
                    case pairs & find \(_, digit) -> digit `isPrefixOf` (x:xs) of
                        Nothing -> filter2 xs
                        Just (c, _) -> c : filter2 xs
    where pairs = zip ['1'..'9'] ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

solve :: MonadIO m => Text -> m ()
solve = aoc parser (solveWith filter1) (solveWith filter2)