module Day09 (solve) where
import           Data.Char (digitToInt)
import           Data.Maybe (isNothing)
import           Data.List (find, sort)
import           Data.Map (Map, (!))
import qualified Data.Map as Map
import           Text.Megaparsec (sepEndBy1, some)
import qualified Text.Megaparsec.Char as P
import           Util (Parser, Point, aocTemplate, adjacentPoints, freqs, listTo2dMap)

parser :: Parser (Map Point Int)
parser = listTo2dMap <$> line `sepEndBy1` P.eol where
        line = some (digitToInt <$> P.digitChar)

flow :: Map Point Int -> Map Point (Maybe Point)
flow m = Map.mapWithKey go m where
    go p v = find
                (\p2 -> Map.findWithDefault 10 p2 m < v)
                (adjacentPoints p)

closure :: Map Point (Maybe Point) -> Map Point Point
closure m = cl where
        cl = Map.mapWithKey (\p -> \case
                     Nothing -> p
                     Just c -> cl ! c  
                  ) m

part1 :: Map Point Int -> Int
part1 m = sum . map ((+1) . (m!) . fst) . filter (isNothing . snd) . Map.toList . flow $ m 

part2 :: Map Point Int -> Int
part2 = product . take 3 . reverse . sort . Map.elems . freqs . Map.elems . closure . flow . Map.filter (<9)

solve :: String -> IO ()
solve = aocTemplate parser pure (pure . part1) (pure . part2)
