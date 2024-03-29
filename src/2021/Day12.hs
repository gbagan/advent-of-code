module Day12 (solve) where
import           AOC.Prelude
import           Data.Char (isUpper)
import           Data.Map ((!))
import qualified Data.Map as Map
import           AOC (aoc)
import           AOC.Parser (Parser, char, eol, letterChar, sepEndBy1, some)

data Edge = Edge String String
type Graph = Map String [String]

parser :: Parser Graph
parser = edgesToGraph <$> sepEndBy1 edge eol where
    edge = Edge <$> some letterChar <* char '-' <*> some letterChar

edgesToGraph :: [Edge] -> Graph
edgesToGraph edges = Map.fromList . map (\u -> (u, neighbor u)) $ vertices where
    vertices = ordNub $ edges >>= \(Edge u v) -> [u, v]
    neighbor u = mapMaybe (\case
                    Edge v w | u == v    -> Just w
                             | u == w    -> Just v
                             | otherwise -> Nothing
                ) edges

part1 :: Graph -> Int
part1 g = go ["start"] "start" where
    go visited current
        | current == "end" = 1
        | otherwise = sum [
            go (if all isUpper nbor then visited else nbor : visited) nbor
            | nbor <- g ! current
            , nbor `notElem` visited
        ]

part2 :: Graph -> Int
part2 g = go ["start"] False "start" where
    go visited visitedTwice current
        | current == "end" = 1
        | otherwise = sum $ do
            nbor <- g ! current
            if visitedTwice then do
                guard $ nbor `notElem` visited
                pure $ go (if all isUpper nbor then visited else nbor : visited) True nbor
            else do
                guard $ nbor /= "start"
                pure $ go (if all isUpper nbor then visited else nbor : visited) (nbor `elem` visited) nbor

solve :: Text -> IO ()
solve = aoc parser part1 part2
