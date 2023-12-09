-- https://adventofcode.com/2022/day/6
module AOC2022.Day06 (solve) where
import           AOC.Prelude
import           Data.List.Split (divvy)
import           AOC (aoc)
import           AOC.Parser (Parser, anySingle, some)
import           AOC.Util (allUnique)

parser :: Parser String
parser = some anySingle

solve' :: Int -> String -> Maybe Int
solve' n = fmap (+n) . findIndex allUnique . divvy n 1

solve :: Text -> IO ()
solve = aoc parser (solve' 4) (solve' 14)
