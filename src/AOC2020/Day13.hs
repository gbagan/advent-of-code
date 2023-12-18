-- https://adventofcode.com/2020/day/13
module AOC2020.Day13 (solve) where
import           AOC.Prelude
import           Data.List (minimum)
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, eol, decimal, sepEndBy1)
import           AOC.Number (chineseRemainder)

data Input = Input Integer [Maybe Integer] 

parser :: Parser Input
parser = Input <$> decimal <* eol <*> period `sepEndBy1` "," where
    period = Just <$> decimal <|> Nothing <$ "x"

part1 :: Input -> Integer
part1 (Input earliest periods) = uncurry (*) $ minimum [((-earliest) `mod` p, p) | p <- catMaybes periods]

part2 :: Input -> Maybe Integer
part2 (Input _ periods) = fst <$> chineseRemainder pairs where
    pairs = mapMaybe f $ zip [0..] periods
    f (i, Just a) = Just (-i, a)
    f _ = Nothing

solve :: Text -> IO ()
solve = aoc parser part1 part2