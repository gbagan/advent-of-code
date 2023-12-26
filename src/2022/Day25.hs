-- https://adventofcode.com/2022/day/25
module Day25 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, choice, eol, sepEndBy1, some)

parser :: Parser [[Int]]
parser = some digit `sepEndBy1` eol where
    digit = choice [0 <$ "0", 1 <$ "1", 2 <$ "2", (-1) <$ "-", (-2) <$ "="]

part1 :: [[Int]] -> String
part1 = reverse . encode . sum . map decode where
    decode = foldl' (\acc i -> acc * 5 + i) 0
    encode 0 = ""
    encode n = encodeDigit d' : encode n'' where
                (n', d) = n `divMod` 5 
                (n'', d') = if d > 2 then (n'+1, d-5) else (n', d) 

    encodeDigit 0 = '0'
    encodeDigit 1 = '1'
    encodeDigit 2 = '2'
    encodeDigit (-1) = '-'
    encodeDigit _ = '='

solve :: Text -> IO ()
solve = aoc parser part1 (const (0::Int))