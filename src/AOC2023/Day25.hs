-- https://adventofcode.com/2023/day/25
module Day25 (solve) where
import           AOC.Prelude hiding (head, last)
import           AOC (aoc)
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as Map
import           AOC.Parser (Parser, sepEndBy1, some, lowerChar, eol, hspace)
import           AOC.Graph.MinCut (removeVertex, minimumCutPhase)

type Network = HashMap Text [Text]

parser :: Parser Network
parser = Map.fromList <$> row `sepEndBy1` eol where
    row = (,) <$> label <* ": " <*> label `sepEndBy1` hspace 
    label = Text.pack <$> some lowerChar 

part1 :: Network -> Int
part1 _ = traceShow (removeVertex 1 g) 0 where
    g :: HashMap Int [(Int, Int)] 
    g = Map.fromList [(0, [(0, 1), (0, 2)]), (1, [(1, 0), (1, 2)]), (2, [(2, 0), (2, 1)])]

part2 :: Network -> Int
part2 _ = 0

solve :: Text -> IO ()
solve = aoc parser part1 part2