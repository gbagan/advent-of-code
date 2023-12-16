-- https://adventofcode.com/2023/day/16
module AOC2023.Day16 (solve) where
import           AOC.Prelude
import           Data.List (maximum)
import qualified Data.HashSet as Set
import           Data.Massiv.Array (Matrix, (!), (!?), B, Comp(Seq), Sz(Sz2))
import qualified Data.Massiv.Array as A
import           Control.Parallel.Strategies (parMap, rdeepseq)
import           AOC (aoc)
import           AOC.V2 (V2(..), toIx2)
import           AOC.Parser (Parser, sepEndBy1, eol, choice, some)
import           AOC.Search (reachableFrom)

data Tile = Empty | Horizontal | Vertical | Slash | Antislash
type Coord = V2 Int
type Direction = V2 Int
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
    Horizontal | drow /= 0 -> [V2 0 (-1), V2 0 1]
    Vertical | dcol /= 0 -> [V2 (-1) 0, V2 1 0]
    _ -> [V2 drow dcol]

neighbors :: Grid -> (Coord, Direction) -> [(Coord, Direction)]
neighbors grid (pos, dir) = [ (nextPos, nextDir)
                            | nextDir <- nextDirections dir (grid ! toIx2 pos)
                            , let nextPos = pos + nextDir
                            , isJust (grid !? toIx2 nextPos)
                            ]

energized :: Grid -> (Coord, Direction) -> Int
energized grid start = Set.size $ Set.map fst reachable where
    reachable = reachableFrom (neighbors grid) start

part1 :: Grid -> Int
part1 grid = energized grid (V2 0 0, V2 0 1)

part2 :: Grid -> Int
part2 grid = maximum $ parMap rdeepseq (energized grid) starts where
    Sz2 h w = A.size grid
    starts = concat $
                [[(V2 r 0, V2 0 1), (V2 r (w-1), V2 0 (-1))] | r <- [0 .. h-1]]
             ++ [[(V2 0 c, V2 1 0), (V2 (h-1) c, V2 (-1) 0)] | c <- [0 .. w-1]]

solve :: Text -> IO ()
solve = aoc parser part1 part2