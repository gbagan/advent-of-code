-- https://adventofcode.com/2022/day/18
module Day18 (solve) where
import           RIO
import           RIO.List (partition)
import           RIO.List.Partial (maximum, minimum)
import qualified RIO.HashSet as Set
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (char, eol)
import           Text.Megaparsec.Char.Lexer (decimal)
import           Linear.V3 (V3(..))
import           Util (Parser, aoc, count)

type Point = V3 Int

parser :: Parser [Point]
parser = point `sepEndBy1` eol where
    point = V3 <$> decimal <* char ',' <*> decimal <* char ',' <*> decimal

directions :: [Point]
directions = [V3 0 0 1, V3 0 1 0, V3 1 0 0] >>= \p -> [p, -p]

part1 :: [Point] -> Int
part1 points = count not [ (p+dir) `Set.member` pset
                         | p <- points
                         , dir <- directions
                         ]
    where pset = Set.fromList points

part2 :: [Point] -> Int
part2 points = dfs [V3 minX minY minZ] Set.empty 0 where
    dfs [] _ counter = counter
    dfs (v:stack) visited counter
        | v `Set.member` visited = dfs stack visited counter
        | otherwise =
            let nbors = filter isInside (map (+v) directions)
                (members, nonMembers) = partition (`Set.member` pset) nbors
            in dfs (nonMembers ++ stack) (Set.insert v visited) (counter + length members)

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

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2