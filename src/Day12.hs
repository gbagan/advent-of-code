module Day12 (solve) where
import           Control.Monad (guard)
import           Data.Char (isUpper)
import           Data.List (nub)
import           Data.Map (Map, (!))
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import           Text.Megaparsec (sepEndBy1, some)
import qualified Text.Megaparsec.Char as P
import           Util (Parser, aocTemplate)

data Edge = Edge String String
type Graph = Map String [String]

parser :: Parser Graph
parser = edgesToGraph <$> sepEndBy1 edge P.eol where
    edge = Edge <$> some P.letterChar <* P.char '-' <*> some P.letterChar

edgesToGraph :: [Edge] -> Graph
edgesToGraph edges = Map.fromList . map (\u -> (u, neighbor u)) $ vertices where
    vertices = nub $ edges >>= \(Edge u v) -> [u, v]
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

solve :: String -> IO ()
solve = aocTemplate parser pure (pure . part1) (pure . part2)
