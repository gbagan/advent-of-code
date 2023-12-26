-- https://adventofcode.com/2023/day/23
module Day23 (solve) where
import           AOC.Prelude hiding (pred)
import           AOC (aoc)
import           Data.Massiv.Array (Matrix, (!), (!?), makeArray, fromLists', size, B, Comp(Seq), Sz(..), Ix2(..))
import           AOC.Parser (Parser, sepEndBy1, eol, some, choice)
import           AOC.V2 (V2(..), adjacent, toIx2)
import           AOC.Graph (longestPath)

data Tile = Path | Forest | North | South | West | East deriving (Eq)
type Grid = Matrix B Tile

parser :: Parser Grid
parser = fromLists' Seq <$> some tile `sepEndBy1` eol where
    tile = choice [Path <$ ".", Forest <$ "#", North <$ "^", South <$ "v", West <$ "<", East <$ ">"]

neighbors1 :: Grid -> V2 Int -> [(V2 Int, Int)]
neighbors1 grid p = case grid ! toIx2 p of
    Path -> [ (p', 1)
            | p' <- adjacent p
            , let tile = grid !? toIx2 p'
            , isJust tile && tile /= Just Forest
            ]
    North -> [(p - V2 1 0, 1)]
    South -> [(p + V2 1 0, 1)]
    West -> [(p - V2 0 1, 1)]
    East -> [(p + V2 0 1, 1)]
    _ -> error "neighbors: cannot happen"

part1 :: Grid -> Int
part1 grid = longestPath neighbors start dest where
    neighbors = neighbors1 grid
    Sz2 h w = size grid
    start = V2 0 1
    dest = V2 (h-1) (w-2)

neighbors2 :: Grid -> V2 Int -> [V2 Int]
neighbors2 grid p = [ p' 
                    | p' <- adjacent p
                    , let tile = grid !? toIx2 p'
                    , isJust tile && tile /= Just Forest
                    ]

compressGrid :: Grid -> Matrix B [(V2 Int, Int)]
compressGrid grid = makeArray Seq (Sz2 h w) \(Ix2 r c) ->
        let pos = V2 r c
            neighbors = neighbors2 grid pos
        in
        if pos == start || pos == dest || length neighbors > 2 then
            [followPath next pos 1 | next <- neighbors]
        else
            []
    where
    followPath pos pred len =
        case neighbors2 grid pos of
            [next1, next2] | next1 == pred -> followPath next2 pos $! len+1
                           | otherwise     -> followPath next1 pos $! len+1
            _ -> (pos, len)

    Sz2 h w = size grid
    start = V2 0 1
    dest = V2 (h-1) (w-2)

part2 :: Grid -> Int
part2 grid = longestPath neighbors start dest where
    compressed = compressGrid grid
    neighbors p = compressed ! toIx2 p
    Sz2 h w = size grid
    start = V2 0 1
    dest = V2 (h-1) (w-2)

solve :: Text -> IO ()
solve = aoc parser part1 part2