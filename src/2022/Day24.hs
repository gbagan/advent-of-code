-- https://adventofcode.com/2022/day/24
module Day24 (solve) where
import           AOC.Prelude
import           AOC.V3 (V3(..))
import           Data.Massiv.Array (Matrix, (!), (!?), fromLists', size, B, Comp(Seq), Ix2(..), Sz(..))
import           AOC (aoc)
import           AOC.Graph (distance)
import           AOC.Parser (Parser, choice, sepEndBy1, eol, some)

data Tile = North | South | West | East | Wall | Empty deriving (Eq)

parser :: Parser (Matrix B Tile)
parser = fromLists' Seq <$> some tile `sepEndBy1` eol where
    tile = choice [North <$ "^", South <$  "v", West <$ "<", East <$ ">", Wall <$ "#", Empty <$ "."]

directions :: [V3 Int]
directions = [V3 1 0 0, V3 1 1 0, V3 1 0 1, V3 1 0 (-1), V3 1 (-1) 0]

travelDuration :: Matrix B Tile -> (Int, Int) -> (Int, Int) -> Int -> Maybe Int
travelDuration grid (startr, startc) dest startTime = distance nborFunc destFunc start
    where
    start = V3 startTime startr startc
    Sz2 nrows ncols = size grid
    destFunc (V3 _ r c) = (r, c) == dest 
    nborFunc v = [v' | dir <- directions, let v' = v + dir, isAllowed v']
    isAllowed (V3 t r c) = (grid !? Ix2 r c) `notElem` [Nothing, Just Wall]
                        && (grid ! Ix2 ((r - t - 1) `mod` (nrows-2) + 1) c) /= South
                        && (grid ! Ix2 ((r + t - 1) `mod` (nrows-2) + 1) c) /= North
                        && (grid ! Ix2 r ((c + t - 1) `mod` (ncols-2) + 1)) /= West
                        && (grid ! Ix2 r ((c - t - 1) `mod` (ncols-2) + 1)) /= East

part1 :: Matrix B Tile -> Maybe Int
part1 grid = travelDuration grid (0, 1) (nrows-1, ncols-2) 0 where
    Sz2 nrows ncols = size grid

part2 :: Matrix B Tile -> Maybe Int
part2 grid = do
    let Sz2 nrows ncols = size grid
    time1 <- travelDuration grid (0, 1) (nrows-1, ncols-2) 0
    time2 <- travelDuration grid (nrows-1, ncols-2) (0, 1) time1
    time3 <- travelDuration grid (0, 1) (nrows-1, ncols-2) (time1+time2)
    pure $ time1 + time2 + time3

solve :: Text -> IO ()
solve = aoc parser part1 part2