-- https://adventofcode.com/2023/day/25
module Day25 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as Map
import           AOC.Parser (Parser, sepEndBy1, some, lowerChar, eol, hspace)
import           AOC.Graph (addEdge, removeEdge, connectedComponents, minimumCut)

type Network = [(Text, [Text])] 

parser :: Parser Network
parser = row `sepEndBy1` eol where
    row = (,) <$> label <* ": " <*> label `sepEndBy1` hspace
    label = Text.pack <$> some lowerChar 

part1 :: Network -> Int
part1 network = product (map length components) where
    graph = foldl' (\g (u, v) -> addEdge u v g) Map.empty
                [(u, v) | (u, nbor) <- network, v <- nbor]
    cutset = minimumCut graph
    graph' = foldl' (\g (u, v) -> removeEdge u v g) graph cutset
    components = connectedComponents graph'

solve :: Text -> IO ()
solve = aoc parser part1 (const (0 :: Int))