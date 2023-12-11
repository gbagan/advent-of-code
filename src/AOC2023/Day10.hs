-- https://adventofcode.com/2023/day/10
module AOC2023.Day10 (solve) where
import           AOC.Prelude hiding (head)
import           Data.List (head, maximum)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import           AOC (aoc)
import           AOC.Parser (Parser, choice, eol, sepEndBy1, some)
import           AOC.Search (bfs)
import           AOC.Util (adjacentPoints, listTo2dMap)
import           AOC.Tuple (thd3)

data Tile = NS | EW | NE | NW | SW | SE | Empty | Start deriving (Eq)
type Coord = (Int, Int)
type Input = [[Tile]]
type Matrix = HashMap Coord Tile

parser :: Parser Input
parser =  some tile `sepEndBy1` eol where
    tile = choice [NS <$ "|", EW <$ "-", NE <$"L", NW <$ "J", SW <$ "7", SE <$ "F", Empty <$ ".", Start <$ "S"]

-- returns the start coordinate and the input where the start tile is replaced with the adequate tile 
getNiceInput :: Input -> (Input, Matrix, Coord)
getNiceInput tiles = (cleanedTiles, cleanedMat, start) where
    start = head [pos | (pos, Start) <- Map.toList mat]
    mat = listTo2dMap tiles
    adequateTile = case [start `elem` neighbors mat nbor | nbor <- neighbors mat start] of
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
neighbors mat (i, j) = case mat Map.!? (i, j) of
    Just NS -> [(i-1, j), (i+1, j)]
    Just EW -> [(i, j-1), (i, j+1)]
    Just NE -> [(i-1, j), (i, j+1)]
    Just NW -> [(i, j-1), (i-1, j)]
    Just SW -> [(i+1, j), (i, j-1)]
    Just SE -> [(i, j+1), (i+1, j)]
    Just Start -> adjacentPoints (i, j)
    _ -> []

part1 :: Input -> Int
part1 tiles = maximum . map fst $ bfs (neighbors mat) start where 
    (_, mat, start) = getNiceInput tiles

-- For part 2, we proceed as follows:
-- First, we replace each tile that belongs to the loop with an empty tile.
-- For each row of the input, we count the number of tiles that are inside the loop (countRow function)
-- There are five possible patterns we can encounter in a row : "|" , "F---7" , "L---J",  "F---J" and "L---7"
--    where "---" is an arbitrary number (possibly 0) of tiles "-" 
-- If we encounter a pattern "F---7" or "L---J", tiles on the left and tiles on the right 
--    of the pattern are all inside or all outside the loop.
-- If we encounter a pattern "|"  or "F---J" or "L---7", then only left tiles or only right tiles are inside the loop.
part2 :: Input -> Int
part2 tiles = sum . map countRow $ cleanedTiles where
    (tiles', mat, start) = getNiceInput tiles
    loopSet = Set.fromList . map snd $ bfs (neighbors mat) start
    -- replace each tile not in the loop with an empty tile
    cleanedTiles = [ [ if (i, j) `Set.member` loopSet then tile else Empty
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