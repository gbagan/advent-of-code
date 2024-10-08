-- https://adventofcode.com/2023/day/16
module Day16 (solve) where
import           AOC.Prelude
import           Data.List (maximum)
import qualified Data.HashSet as Set
import           Data.Massiv.Array (Matrix, (!), (!?), B, Comp(Seq), Sz(Sz2))
import qualified Data.Massiv.Array as A
-- import           Control.Parallel.Strategies (parListChunk, rdeepseq, using)
import           AOC (aoc)
import           AOC.V2 (V2(..), origin, north, south, west, east, toIx2)
import           AOC.Parser (Parser, sepEndBy1, eol, choice, some)
import           AOC.Graph (reachableFrom)

data Tile = Empty | Horizontal | Vertical | Slash | Antislash
type Position = V2 Int
type Direction = V2 Int
type Beam = (Position, Direction)
type Grid = Matrix B Tile

parser :: Parser Grid 
parser = A.fromLists' Seq <$> some tile `sepEndBy1` eol where
    tile = choice [ Empty <$ "."
                  , Horizontal <$ "-"
                  , Vertical <$ "|"
                  , Slash <$"/"
                  , Antislash <$ "\\"
                  ]

nextDirections :: Direction -> Tile -> [Direction]
nextDirections (V2 drow dcol) = \case
    Slash -> [V2 (-dcol) (-drow)]
    Antislash -> [V2 dcol drow]
    Horizontal | drow /= 0 -> [west, east]
    Vertical | dcol /= 0 -> [north, south]
    _ -> [V2 drow dcol]

neighbors :: Grid -> Beam -> [Beam]
neighbors grid (pos, dir) = [ (nextPos, nextDir)
                            | nextDir <- nextDirections dir (grid ! toIx2 pos)
                            , let nextPos = pos + nextDir
                            , isJust (grid !? toIx2 nextPos)
                            ]

energized :: Grid -> Beam -> Int
energized grid start = Set.size (Set.map fst reachable) where
    reachable = reachableFrom (neighbors grid) start

part1 :: Grid -> Int
part1 grid = energized grid (origin, east)

part2 :: Grid -> Int
part2 grid = maximum (map (energized grid) starts) where
    Sz2 h w = A.size grid
    starts = concat $
                [[(V2 r 0, east), (V2 r (w-1), west)] | r <- [0 .. h-1]]
             ++ [[(V2 0 c, south), (V2 (h-1) c, north)] | c <- [0 .. w-1]]

solve :: Text -> IO ()
solve = aoc parser part1 part2