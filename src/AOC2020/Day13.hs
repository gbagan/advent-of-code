-- https://adventofcode.com/2020/day/13
module AOC2020.Day13 (solve) where
import           AOC.Prelude
import           Data.List (minimum)
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, eol, decimal, sepEndBy1)

data Input = Input Int [Maybe Int] 

parser :: Parser Input
parser = Input <$> decimal <* eol <*> period `sepEndBy1` "," where
    period = Just <$> decimal <|> Nothing <$ "x"

part1 :: Input -> Int
part1 (Input earliest periods) = uncurry (*) $ minimum [((-earliest) `mod` p, p) | p <- catMaybes periods]

part2 :: Input -> Int
part2 (Input earliest periods) = 0

solve :: Text -> IO ()
solve = aoc parser part1 part2