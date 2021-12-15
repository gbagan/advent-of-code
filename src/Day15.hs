module Day15 (solve) where
import           Control.Monad ((<=<))
import           Data.Char (digitToInt)
import qualified Data.Set as Set
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import           Text.Megaparsec (sepEndBy1, some)
import qualified Text.Megaparsec.Char as P
import           Util (Parser, Point, aocTemplate, adjacentPoints, listTo2dMap)

parser :: Parser (Map Point Int)
parser = listTo2dMap <$> line `sepEndBy1` P.eol where
        line = some (digitToInt <$> P.digitChar)

dijkstra :: (Ord v, Real w) => (v -> [(v, w)]) -> v -> v -> Maybe w
dijkstra nbors source target = go Set.empty (Set.singleton (0, source)) where
    go visited queue = case Set.minView queue of
        Nothing -> Nothing
        Just ((cost, v), queue')
            | v == target          -> Just cost
            | Set.member v visited -> go visited queue'
            | otherwise            -> go
                                        (Set.insert v visited)
                                        (foldr Set.insert queue' $ (\(u, w) -> (w+cost, u)) <$> nbors v)

neighbors :: Map Point Int -> Point -> [(Point, Int)]
neighbors mp v = mapMaybe
                    (\u -> (u,) <$> Map.lookup u mp)
                    (adjacentPoints v)

part1 :: Map Point Int -> Maybe Int
part1 mp = dijkstra (neighbors mp) (0, 0) (99, 99)

duplicateGrid :: Map Point Int -> Map Point Int
duplicateGrid = Map.fromList . (( \((x, y), w) -> [ ((x + 100 * i, y + 100 * j), nm (w+i+j)) | i <- [0..4], j <- [0..4] ]) <=< Map.toList)
    where nm x = if x > 9 then x - 9 else x

part2 :: Map Point Int -> Maybe Int
part2 mp = dijkstra (neighbors mp') (0, 0) (499, 499) where
        mp' = duplicateGrid mp

solve :: String -> IO ()
solve = aocTemplate parser part1 part2