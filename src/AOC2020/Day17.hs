-- https://adventofcode.com/2020/day/17
module AOC2020.Day17 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, eol, some)
import qualified Data.HashSet as Set 
import qualified Data.HashMap.Strict as Map
import           AOC.Util (times, listTo2dSet)
import           AOC.V2 (V2(..))
import           AOC.V3 (V3(..))
import qualified AOC.V3 as V3
import           AOC.V4 (V4(..))
import qualified AOC.V4 as V4

type Grid2 = HashSet (V2 Int)

unionsWith :: Hashable a => (b -> b -> b) -> [HashMap a b] -> HashMap a b
unionsWith f = foldl' (Map.unionWith f) Map.empty

step :: Hashable a => (a -> [a]) -> HashSet a -> HashSet a
step nbor grid = 
    Map.keysSet
    . Map.filterWithKey check
    . unionsWith (+)
    . map (Map.fromList . map (,1::Int) . nbor)
    $ Set.toList grid
    where
    check pos val = val == 3 || val == 2 && Set.member pos grid

parser :: Parser Grid2
parser = listTo2dSet <$> some cube `sepEndBy1` eol where
    cube = False <$ "." <|> True <$ "#"

solveFor :: Hashable a => (V2 Int -> a) -> (a -> [a]) -> Grid2 -> Int
solveFor to nbor grid = Set.size $ times 6 (step nbor) (Set.map to grid)

part1 :: Grid2 -> Int
part1 = solveFor toV3 V3.surrounding where
    toV3 (V2 x y) = V3 x y 0

part2 :: Grid2 -> Int
part2 = solveFor toV4 V4.surrounding where
    toV4 (V2 x y) = V4 x y 0 0

solve :: Text -> IO ()
solve = aoc parser part1 part2