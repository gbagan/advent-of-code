-- https://adventofcode.com/2022/day/18
module Day18 (solve) where
import           AOC.Prelude
import           Data.List (maximum, minimum)
import qualified Data.HashSet as Set
import           AOC.V3 (V3(..))
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, eol, decimal)
import           AOC.Search (dfsM)
import           AOC.List (count)

type Point = V3 Int

parser :: Parser [Point]
parser = point `sepEndBy1` eol where
    point = V3 <$> decimal <* "," <*> decimal <* "," <*> decimal

directions :: [Point]
directions = [V3 0 0 1, V3 0 1 0, V3 1 0 0] >>= \p -> [p, -p]

part1 :: [Point] -> Int
part1 points = count not [ (p+dir) `Set.member` pset
                         | p <- points
                         , dir <- directions
                         ]
    where pset = Set.fromList points

part2 :: [Point] -> Int
part2 points = execState (dfsM nborFunc (V3 minX minY minZ)) 0 where
    nborFunc v = do
        let nbors = filter isInside (map (+v) directions)
        let (members, nonMembers) = partition (`Set.member` pset) nbors
        modify' (+ length members)
        pure nonMembers

    pset = Set.fromList points
    minX = minimum [x | V3 x _ _ <- points] - 1
    minY = minimum [y | V3 _ y _ <- points] - 1
    minZ = minimum [z | V3 _ _ z <- points] - 1
    maxX = maximum [x | V3 x _ _ <- points] + 1
    maxY = maximum [y | V3 _ y _ <- points] + 1
    maxZ = maximum [z | V3 _ _ z <- points] + 1

    isInside (V3 x y z) = minX <= x && x <= maxX
                       && minY <= y && y <= maxY
                       && minZ <= z && z <= maxZ

solve :: Text -> IO ()
solve = aoc parser part1 part2