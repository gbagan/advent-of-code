-- https://adventofcode.com/2019/day/4
module Day04 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, decimal)
import           AOC.List (count)

parser :: Parser (Int, Int)
parser = (,) <$> decimal <* "-" <*> decimal

nonDecreasing :: String -> Bool
nonDecreasing s = and $ zipWith (<=) s (drop 1 s)

twoAdjacent, twoAdjacent' :: String -> Bool
twoAdjacent = any ((> 1) . length) . group
twoAdjacent' = any ((==2) . length) . group

solveWith :: (String -> Bool) -> (Int, Int) -> Int
solveWith check (min_, max_) = count (\txt -> nonDecreasing txt && check txt) . map show $ [min_..max_]

solve :: Text -> IO ()
solve = aoc parser (solveWith twoAdjacent) (solveWith twoAdjacent')