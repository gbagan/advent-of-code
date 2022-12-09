module Day15 (solve) where
import           RIO hiding (some)
import           RIO.Char.Partial (digitToInt)
import           Text.Megaparsec (sepEndBy1, some)
import           Text.Megaparsec.Char (digitChar, eol)
import           Util (Parser, Point, aoc, adjacentPoints, dijkstra, Matrix, (!!!), (!!?), matFromList, matGenerate)

parser :: Parser (Matrix Int)
parser = matFromList <$> line `sepEndBy1` eol where
    line = some (digitToInt <$> digitChar)

neighbors :: Matrix Int -> Point -> [(Point, Int)]
neighbors mat v = mapMaybe (\u -> (u,) <$> mat !!? u) (adjacentPoints v)

part1 :: Matrix Int -> Maybe Int
part1 mp = dijkstra (neighbors mp) (0, 0) (99, 99)

duplicateGrid :: Matrix Int -> Matrix Int
duplicateGrid m = matGenerate 500 500 \i j -> nm $ m !!! (i `mod` 100, j `mod` 100) + i `div` 100 + j `div` 100
    where nm x = if x > 9 then x - 9 else x

part2 :: Matrix Int -> Maybe Int
part2 mat = dijkstra (neighbors mat') (0, 0) (499, 499) where
        mat' = duplicateGrid mat

solve :: (HasLogFunc env) => Text -> RIO env ()
solve = aoc parser part1 part2