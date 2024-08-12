-- https://adventofcode.com/2015/day/1
module Day09 (solve) where
import           AOC.Prelude
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import           AOC (aoc')
import           AOC.List (drop1, minimum, maximum)
import           AOC.Parser (Parser, decimal, eol, sepEndBy1, some, letterChar)
import           AOC.Tuple (fst3, snd3)

type City = Text
type Network = HashMap (City, City) Int
type Input = ([City], Network)

parser :: Parser [(City, City, Int)]
parser = row `sepEndBy1` eol where
    city = Text.pack <$> some letterChar
    row = (,,) <$> city <* " to " <*> city <* " = " <*> decimal

precomp :: [(Text, Text, Int)] -> Input
precomp rows = (cities, network) where
    cities = ordNub (map fst3 rows ++ map snd3 rows) 
    network = foldl' (flip go) Map.empty rows
    go (city1, city2, len) = Map.insert (city1, city2) len . Map.insert (city2, city1) len

solveFor :: ([Int] -> Int) -> Input -> Int
solveFor f (cities, network) = f . map distance $ permutations cities where
    distance xs = sum [network Map.! (x, y) | (x, y) <- zip xs (drop1 xs)]

solve :: Text -> IO ()
solve = aoc' parser (pure . precomp) (solveFor minimum) (solveFor maximum)
