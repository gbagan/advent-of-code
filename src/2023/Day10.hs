-- https://adventofcode.com/2023/day/10
module Day10 (solve) where
import           AOC.Prelude
import qualified Data.HashMap.Strict as Map
import           AOC (aoc')
import           AOC.List (headMaybe)
import           AOC.Parser (Parser, choice, eol, sepEndBy1, some)
import           AOC.V2 (V2(..), adjacent)
import           AOC.Util (listTo2dMap, shoelaceFormula)

data Tile = NS | EW | NE | NW | SW | SE | Empty | Start deriving (Eq, Show)
type Coord = V2 Int
type Input = [[Tile]]
type Matrix = HashMap Coord Tile

parser :: Parser Input  
parser = some tile `sepEndBy1` eol where
    tile = choice [NS <$ "|", EW <$ "-", NE <$"L", NW <$ "J", SW <$ "7", SE <$ "F", Empty <$ ".", Start <$ "S"]

-- returns the start coordinate and the input where the start tile is replaced with the adequate tile 
getNiceInput :: Input -> Maybe (Matrix, Coord)
getNiceInput tiles = do
    let mat = listTo2dMap tiles
    start <- headMaybe [pos | (pos, Start) <- Map.toList mat]
    let adequateTile = case [start `elem2` neighbors mat nbor | nbor <- adjacent start] of
            -- (x-1, y), (x+1, y), (x, y-1), (x, y+1)
            [False, False, True, True] -> EW
            [False, True, False, True] -> SE
            [False, True, True, False] -> SW
            [True, False, False, True] -> NE
            [True, False, True, False] -> NW
            [True, True, False, False] -> NS       
            _ -> Empty  -- cannot happen if the input is nice
    let cleanedMat = Map.insert start adequateTile mat
    pure (cleanedMat, start)
    where
    elem2 x (y, z) = x == y || x == z 

neighbors :: Matrix -> Coord -> (Coord, Coord)
neighbors mat (V2 i j) = case mat Map.!? V2 i j of
    Just NS -> (V2 (i-1) j, V2 (i+1) j)
    Just EW -> (V2 i (j-1), V2 i (j+1))
    Just NE -> (V2 (i-1) j, V2 i (j+1))
    Just NW -> (V2 i (j-1), V2 (i-1) j)
    Just SW -> (V2 (i+1) j, V2 i (j-1))
    Just SE -> (V2 i (j+1), V2 (i+1) j)
    _ -> error "neighbors: cannot happen"

loopList :: Matrix -> Coord -> [Coord]
loopList mat start = go (prevStart, start)
    where
    (prevStart, _) = neighbors mat start
    go (prev', current) | current == prevStart = [current]
                        | next1 == prev'       = current : go (current, next2)
                        | otherwise            = current : go (current, next1)
            where (next1, next2) = neighbors mat current
        
part1 :: (Matrix, Coord) -> Int
part1 (mat, start) = length (loopList mat start) `div` 2

part2 :: (Matrix, Coord) -> Int
part2 (mat, start) = (doubleArea - boundary) `div` 2 + 1 where
    loop = loopList mat start
    doubleArea = shoelaceFormula loop
    boundary = length loop

solve :: Text -> IO ()
solve = aoc' parser getNiceInput part1 part2