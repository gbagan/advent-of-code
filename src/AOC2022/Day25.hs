-- https://adventofcode.com/2022/day/25
module AOC2022.Day25 (solve) where
import           RIO
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (char, eol)
import           Util (Parser, aoc)

parser :: Parser [[Int]]
parser = some digit `sepEndBy1` eol where
    digit = 0 <$ char '0' <|> 1 <$ char '1' <|> 2 <$ char '2'
            <|> (-1) <$ char '-' <|> (-2) <$ char '='

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



solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 (const (0::Int))