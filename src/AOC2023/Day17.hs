-- https://adventofcode.com/2023/day/17
module Day17 (solve) where
import           AOC.Prelude
import           Data.Char (digitToInt)
import           Data.Massiv.Array (Matrix, (!), (!?), U, Comp(Seq), Sz(Sz2))
import qualified Data.Massiv.Array as A
import           AOC (aoc)
import           AOC.V2 (V2(..), toIx2)
import           AOC.Parser (Parser, sepEndBy1, eol, digitChar, some)
import           AOC.Graph (dijkstra')  

type Grid = Matrix U Int
type Position = V2 Int
type Direction = Bool -- True -> horizontal | False -> vertical

parser :: Parser Grid
parser = A.fromLists' Seq <$> some (digitToInt <$> digitChar) `sepEndBy1` eol

neighbors :: [Int] -> Grid -> (Position, Direction) -> [((Position, Direction), Int)]
neighbors nbSteps grid (pos, dir) =
    [ ((nextPos, not dir), weight)
    | let vDir = if dir then V2 0 1 else V2 1 0
    , i <- nbSteps
    , let nextPos = pos + fmap (*i) vDir
    , isJust (grid !? toIx2 nextPos)
    , let range = if i > 0 then [1..i] else [i..(-1)]
    , let weight = sum [grid ! toIx2 (pos + fmap (*j) vDir) | j <- range]
    ]

solveFor :: [Int] -> Grid -> Maybe Int
solveFor nbSteps grid = dijkstra' (neighbors nbSteps' grid) (`elem` ends) starts where
    nbSteps' = nbSteps ++ map negate nbSteps
    Sz2 h w = A.size grid
    starts = (V2 0 0,) <$> [True, False]
    ends = (V2 (h-1) (w-1),) <$> [True, False]

solve :: Text -> IO ()
solve = aoc parser (solveFor [1..3]) (solveFor [4..10])