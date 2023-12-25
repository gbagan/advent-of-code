-- https://adventofcode.com/2020/day/17
module Day17 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, eol, some)
import qualified Data.HashSet as Set 
import qualified AOC.HashMap as Map
import           AOC.Util (times, listTo2dSet)
import           AOC.V2 (V2(..))
import           AOC.V3 (V3(..))
import qualified AOC.V3 as V3
import           AOC.V4 (V4(..))
import qualified AOC.V4 as V4

type Grid2 = HashSet (V2 Int)

rule :: Hashable a => HashSet a -> a -> Int -> Bool
rule grid pos val = val == 3 || val == 2 && Set.member pos grid

step :: Hashable a => (a -> [a]) -> HashSet a -> HashSet a
step nbor grid = 
    Map.keysSet
    . Map.filterWithKey (rule grid)
    . Map.unionsWith (+)
    . map (Map.fromList . map (,1) . nbor)
    $ Set.toList grid

parser :: Parser Grid2
parser = listTo2dSet <$> some cube `sepEndBy1` eol where
    cube = False <$ "." <|> True <$ "#"

solveFor :: Hashable a => (V2 Int -> a) -> (a -> [a]) -> Grid2 -> Int
solveFor convert nbor grid = Set.size $ times 6 (step nbor) (Set.map convert grid)

part1 :: Grid2 -> Int
part1 = solveFor toV3 V3.surrounding where
    toV3 (V2 x y) = V3 x y 0

part2 :: Grid2 -> Int
part2 = solveFor toV4 V4.surrounding where
    toV4 (V2 x y) = V4 x y 0 0

solve :: Text -> IO ()
solve = aoc parser part1 part2