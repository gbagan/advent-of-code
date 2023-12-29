-- https://adventofcode.com/2016/day/10
module Day10 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, decimal, eol, sepEndBy1, format)

data Bin = Bot !Int | Output !Int
data Instr = Goes !Int !Int | Gives !Int !Bin !Bin

parser :: Parser [Instr]
parser = (goes <|> give) `sepEndBy1` eol where
    goes = [format| $Goes value {decimal} goes to bot {decimal}|]
    give = [format| $giveF bot {decimal} gives low to {bin} {decimal} and high to {bin} {decimal}|]
    bin = Bot <$ "bot" <|> Output <$ "output"
    giveF giver lowBin lowInt highBin highInt = Gives giver (lowBin lowInt) (highBin highInt)

part1 :: [Instr] -> Int
part1 _ = 0

part2 :: [Instr] -> Int
part2 _ = 0

solve :: Text -> IO ()
solve = aoc parser part1 part2