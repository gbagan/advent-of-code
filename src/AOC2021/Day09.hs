module AOC2021.Day09 (solve) where
import           AOC.Prelude
import           Data.Char (digitToInt)
import qualified Data.HashMap.Strict as HMap
import           Data.Map.Lazy ((!))
import qualified Data.Map.Lazy as LMap
import           AOC (aoc)
import           AOC.Parser (Parser, digitChar, eol, sepEndBy1, some)
import           AOC.V2 (V2(..), adjacent)
import           AOC.Util (freqs)

type Coord = V2 Int

parser :: Parser (Map Coord Int)
parser = listTo2dMap <$> line `sepEndBy1` eol where
        line = some (digitToInt <$> digitChar)

listTo2dMap :: [[a]] -> Map Coord a
listTo2dMap l =
    LMap.fromList
        [( V2 i j, v)
        | (i, row) <- zip [0..] l
        , (j, v) <- zip [0..] row
        ]

flow :: Map Coord Int -> Map Coord (Maybe Coord)
flow m = LMap.mapWithKey go m where
    go p v = find
                (\p2 -> LMap.findWithDefault 10 p2 m < v)
                (adjacent p)

closure :: Map Coord (Maybe Coord) -> Map Coord Coord
closure m = cl where
        cl = LMap.mapWithKey (\p -> \case
                     Nothing -> p
                     Just c -> cl ! c
                  ) m

part1 :: Map Coord Int -> Int
part1 m = sum [m ! p + 1 | (p, Nothing) <- LMap.toList (flow m)] 

part2 :: Map Coord Int -> Int
part2 = product . take 3 . sortOn Down . HMap.elems . freqs . LMap.elems . closure . flow . LMap.filter (<9)

solve :: Text -> IO ()
solve = aoc parser part1 part2
