-- https://adventofcode.com/2015/day/8
module Day08 (solve) where
import           AOC.Prelude hiding (init, tail)
import           Data.List (init, tail)
import           AOC (aoc)
import           AOC.Parser (Parser, alphaNumChar, char, eol, sepEndBy1, some)
import           AOC.List (count)

parser :: Parser [String]
parser = line `sepEndBy1` eol where
    line = some (alphaNumChar <|> char '"' <|> char '\\')

diff1, diff2 :: String -> Int
diff1 s = 2 + go (init (tail s)) where
    go []                = 0
    go ('\\':'"':xs)     = 1 + go xs
    go ('\\':'\\':xs)    = 1 + go xs
    go ('\\':'x':_:_:xs) = 3 + go xs
    go (_:xs)            = go xs
diff2 s = 2 + count (`elem` ['\\', '"']) s

solveFor :: (String -> Int) -> [String] -> Int
solveFor diff = sum . map diff where

solve :: Text -> IO ()
solve = aoc parser (solveFor diff1) (solveFor diff2)
