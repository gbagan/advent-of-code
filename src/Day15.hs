module Day15 (solve) where
import           RIO hiding (some)
import           RIO.Char.Partial (digitToInt)
import qualified RIO.Map as Map
import           Text.Megaparsec (sepEndBy1, some)
import qualified Text.Megaparsec.Char as P
import           Util (Parser, Point, aocTemplate, adjacentPoints, dijkstra, listTo2dMap)

parser :: Parser (Map Point Int)
parser = listTo2dMap <$> line `sepEndBy1` P.eol where
        line = some (digitToInt <$> P.digitChar)

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

solve :: (HasLogFunc env) => Text -> RIO env ()
solve = aocTemplate parser pure part1 part2