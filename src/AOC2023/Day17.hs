-- https://adventofcode.com/2023/day/17
module AOC2023.Day17 (solve) where
import           AOC.Prelude
import           Data.Char (digitToInt)
import           Data.Massiv.Array (Matrix, (!), (!?), U, Comp(Seq), Sz(Sz2))
import qualified Data.Massiv.Array as A
import           AOC (aoc)
import           AOC.V2 (V2(..), adjacent, toIx2)
import           AOC.Parser (Parser, sepEndBy1, eol, digitChar, some)
import           AOC.Search (dijkstra')

type Grid = Matrix U Int
type Position = V2 Int
type Direction = V2 Int

directions :: [V2 Int]
directions = adjacent (V2 0 0)

parser :: Parser Grid
parser = A.fromLists' Seq <$> some (digitToInt <$> digitChar) `sepEndBy1` eol

neighbors :: [Int] -> Grid -> (Position, Direction) -> [((Position, Direction), Int)]
neighbors nbSteps grid (pos, dir) =
    [ ((nextPos, nextDir), weight)
    | i <- nbSteps
    , nextDir <- directions
    , nextDir /= dir && nextDir /= -dir
    , let nextPos = pos + fmap (*i) nextDir
    , isJust (grid !? toIx2 nextPos)
    , let weight = sum [grid ! toIx2 (pos + fmap (*j) nextDir) | j <- [1..i]]
    ]

solveFor :: [Int] -> Grid -> Maybe Int
solveFor nbSteps grid = dijkstra' (neighbors nbSteps grid) (`elem` ends) starts where
    Sz2 h w = A.size grid
    starts = [(V2 0 0, V2 1 0), (V2 0 0, V2 0 1)]
    ends = [(V2 (h-1) (w-1), V2 0 1), (V2 (h-1) (w-1), V2 1 0)]

solve :: Text -> IO ()
solve = aoc parser (solveFor [1..3]) (solveFor [4..10])