-- https://adventofcode.com/2018/day/6
module Day06 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, eol, decimal, scanf)
import           AOC.V2 (V2(..), adjacent, manhattan)
import           AOC.List (count, median, minimumOn)
import qualified AOC.Area as Area
import           AOC.Graph (bfs)
import qualified Data.IntMap.Strict as Map
import qualified Data.IntSet as Set
import           Data.List (maximum)

parser :: Parser [V2 Int]
parser = [scanf|$V2 {decimal}, {decimal}|] `sepEndBy1` eol

nearest :: [V2 Int] -> V2 Int -> Maybe Int
nearest vs pos = if nbMin > 1 then Nothing else Just min_ where
    dists = zipWith (\i v -> (i, manhattan v pos)) [0..] vs
    (min_, minDist) = minimumOn snd dists
    nbMin = count (\(_, d) -> d == minDist) dists

part1 :: [V2 Int] -> Int
part1 xs = maximum [size | (idx, size) <- Map.toList areas, idx `Set.notMember` borders] where
    box = Area.boundingBox xs 
    (areas, borders) = foldl' go (Map.empty, Set.empty) (Area.elements box)
    go (areas', borders') pos = case nearest xs pos of
        Nothing -> (areas', borders')
        Just idx -> 
            if pos `Area.isBorderOf` box
                then (areas', Set.insert idx borders')
                else (Map.insertWith (+) idx (1::Int) areas', borders')

part2 :: [V2 Int] -> Int
part2 vs = length $ bfs nbors starting where
    starting = V2 (median (map _x vs)) (median (map _y vs))
    nbors v = [v' | v' <- adjacent v, sumOfDistances v' < 10000]
    sumOfDistances v = sum (map (manhattan v) vs)

solve :: Text -> IO ()
solve = aoc parser part1 part2