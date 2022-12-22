module AOC2021.Day15 (solve) where
import           RIO hiding (some)
import           RIO.Char.Partial (digitToInt)
import           Text.Megaparsec (sepEndBy1, some)
import           Text.Megaparsec.Char (digitChar, eol)
import           Util (Parser, Point, aoc, adjacentPoints)
import           Util.Matrix (Matrix, (!), (!?))
import qualified Util.Matrix as M
import           Util.Search (dijkstra)

parser :: Parser (Matrix Int)
parser = M.fromList <$> line `sepEndBy1` eol where
    line = some (digitToInt <$> digitChar)

neighbors :: Matrix Int -> Point -> [(Point, Int)]
neighbors mat v = mapMaybe (\u -> (u,) <$> mat !? u) (adjacentPoints v)

part1 :: Matrix Int -> Maybe Int
part1 mp = dijkstra (neighbors mp) (== (99, 99)) (0, 0) 

duplicateGrid :: Matrix Int -> Matrix Int
duplicateGrid m = M.generate 500 500 \i j -> nm $ m ! (i `mod` 100, j `mod` 100) + i `div` 100 + j `div` 100
    where nm x = if x > 9 then x - 9 else x

part2 :: Matrix Int -> Maybe Int
part2 mat = dijkstra (neighbors mat') (== (499, 499)) (0, 0)  where
        mat' = duplicateGrid mat

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2