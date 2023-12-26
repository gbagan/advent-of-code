-- https://adventofcode.com/2020/day/25
module Day25 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, eol, decimal)
import           AOC.Number (power, discreteLogarithm)

parser :: Parser (Integer, Integer)
parser = (,) <$> decimal <* eol <*> decimal

modulo :: Integer
modulo = 20201227

mul :: Integer -> Integer -> Integer
mul x y = x * y `rem` modulo

part1 :: (Integer, Integer) -> Maybe Integer
part1 (cardPublicKey, doorPublicKey) = do
    cardExponent <- discreteLogarithm 7 modulo cardPublicKey
    Just $ power mul doorPublicKey cardExponent

solve :: Text -> IO ()
solve = aoc parser part1 (const (0 ::Int))