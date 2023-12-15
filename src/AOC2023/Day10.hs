-- https://adventofcode.com/2023/day/10
module AOC2023.Day10 (solve) where
import           AOC.Prelude hiding (head)
import           Data.List (head, maximum)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import           AOC (aoc)
import           AOC.Parser (Parser, choice, eol, sepEndBy1, some)
import           AOC.Search (bfs)
import           AOC.V2 (V2(..), adjacent)
import           AOC.Util (listTo2dMap)
import           AOC.Tuple (thd3)

data Tile = NS | EW | NE | NW | SW | SE | Empty | Start deriving (Eq)
type Coord = V2 Int
type Input = [[Tile]]
type Matrix = HashMap Coord Tile

parser :: Parser Input  
parser = some tile `sepEndBy1` eol where
    tile = choice [NS <$ "|", EW <$ "-", NE <$"L", NW <$ "J", SW <$ "7", SE <$ "F", Empty <$ ".", Start <$ "S"]

-- returns the start coordinate and the input where the start tile is replaced with the adequate tile 
getNiceInput :: Input -> (Input, Matrix, Coord)
getNiceInput tiles = (cleanedTiles, cleanedMat, start) where
    start = head [pos | (pos, Start) <- Map.toList mat]
    mat = listTo2dMap tiles
    adequateTile = case [start `elem` neighbors mat nbor | nbor <- adjacent start] of
        -- (x-1, y), (x+1, y), (x, y-1), (x, y+1)
        [True, True, False, False] -> NS
        [False, False, True, True] -> EW
        [True, False, False, True] -> NE
        [True, False, True, False] -> NW
        [False, True, False, True] -> SE
        [False, True, True, False] -> SW
        _ -> Empty  -- cannot happen if the input is nice
    cleanedMat = Map.insert start adequateTile mat
    cleanedTiles = [ [ if tile == Start then adequateTile else tile | tile <- row] 
                   | row <- tiles
                   ]

neighbors :: Matrix -> Coord -> [Coord]
neighbors mat (V2 i j) = case mat Map.!? V2 i j of
    Just NS -> [V2 (i-1) j, V2 (i+1) j]
    Just EW -> [V2 i (j-1), V2 i (j+1)]
    Just NE -> [V2 (i-1) j, V2 i (j+1)]
    Just NW -> [V2 i (j-1), V2 (i-1) j]
    Just SW -> [V2 (i+1) j, V2 i (j-1)]
    Just SE -> [V2 i (j+1), V2 (i+1) j]
    _ -> []

part1 :: Input -> Int
part1 tiles = maximum . map fst $ bfs (neighbors mat) start where 
    (_, mat, start) = getNiceInput tiles

part2 :: Input -> Int
part2 tiles = sum . map countRow $ cleanedTiles where
    (tiles', mat, start) = getNiceInput tiles
    loopSet = Set.fromList . map snd $ bfs (neighbors mat) start
    -- replace each tile not in the loop with an empty tile
    cleanedTiles = [ [ if V2 i j `Set.member` loopSet then tile else Empty
                     | (j, tile) <- zip [0..] row
                     ] 
                   | (i, row) <- zip [0..] tiles'
                   ]
    countRow = thd3 . foldl' go (False, False, 0)
    go (isInside, fromNorth, counter) = \case
        NS -> (not isInside, fromNorth, counter)
        NE -> (isInside, True, counter)
        SE -> (isInside, False, counter)
        NW -> (isInside == fromNorth, fromNorth, counter)
        SW -> (isInside /= fromNorth, fromNorth, counter)
        Empty -> (isInside, fromNorth, if isInside then counter+1 else counter)
        _ -> (isInside, fromNorth, counter)

solve :: Text -> IO ()
solve = aoc parser part1 part2