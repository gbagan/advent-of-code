module AOC2021.Day09 (solve) where
import           Relude hiding (some)
import           Data.Char (digitToInt)
import qualified Data.HashMap.Strict as HMap
import           Data.Map.Lazy ((!))
import qualified Data.Map.Lazy as LMap
import           Text.Megaparsec (sepEndBy1, some)
import           Text.Megaparsec.Char (digitChar, eol)
import           Util (Parser, Point, aoc, adjacentPoints, freqs)

parser :: Parser (Map Point Int)
parser = listTo2dMap <$> line `sepEndBy1` eol where
        line = some (digitToInt <$> digitChar)

listTo2dMap :: [[a]] -> Map (Int, Int) a
listTo2dMap l =
    LMap.fromList
        [((i, j), v)
        | (j, row) <- zip [0..] l
        , (i, v) <- zip [0..] row
        ]

flow :: Map Point Int -> Map Point (Maybe Point)
flow m = LMap.mapWithKey go m where
    go p v = find
                (\p2 -> LMap.findWithDefault 10 p2 m < v)
                (adjacentPoints p)

closure :: Map Point (Maybe Point) -> Map Point Point
closure m = cl where
        cl = LMap.mapWithKey (\p -> \case
                     Nothing -> p
                     Just c -> cl ! c
                  ) m

part1 :: Map Point Int -> Int
part1 m = sum . map ((+1) . (m!) . fst) . filter (isNothing . snd) . LMap.toList . flow $ m 

part2 :: Map Point Int -> Int
part2 = product . take 3 . sortOn Down . HMap.elems . freqs . LMap.elems . closure . flow . LMap.filter (<9)

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2
