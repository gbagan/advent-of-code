-- https://adventofcode.com/2023/day/10
module AOC2023.Day10 (solve) where
import           AOC.Prelude hiding (head)
import           Data.List (head, maximum)
import           Data.List.Split (splitWhen)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import           AOC (aoc)
import           AOC.Parser (Parser, choice, eol, sepEndBy1, some)
import           AOC.Search (bfs)
import           AOC.Util (adjacentPoints, listTo2dMap)

data Tile = NS | EW | NE | NW | SW | SE | NoPipe | Start deriving (Eq)
type Index2 = (Int, Int)
type Input = [[Tile]]
type Matrix = HashMap Index2 Tile

parser :: Parser Input
parser =  some tile `sepEndBy1` eol where
    tile = choice [NS <$ "|", EW <$ "-", NE <$"L", NW <$ "J", SW <$ "7", SE <$ "F", NoPipe <$ ".", Start <$ "S"]

cleanInput :: Input -> (Input, Matrix, (Int, Int))
cleanInput tiles = (cleanedTiles, cleanedMat, start) where
    start = head [pos | (pos, Start) <- Map.toList mat]
    mat = listTo2dMap tiles
    goodTile = case [start `elem` neighbors mat nbor | nbor <- neighbors mat start] of
        -- [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
        [True, True, False, False] -> NS
        [False, False, True, True] -> EW
        [True, False, False, True] -> NE
        [True, False, True, False] -> NW
        [False, True, False, True] -> SE
        [False, True, True, False] -> SW
        _ -> NoPipe  -- not possible
    cleanedMat = Map.insert start goodTile mat
    cleanedTiles = [ [ if tile == Start then goodTile else tile | tile <- row] 
                   | row <- tiles
                   ]

neighbors :: Matrix -> Index2 -> [Index2]
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
    (_, mat, start) = cleanInput tiles

keepOdds :: [a] -> [a]
keepOdds (_:y:xs) = y : keepOdds xs
keepOdds _ = []

part2 :: Input -> Int
part2 tiles = sum . map countRow $ cleanedTiles where
    (tiles', mat, start) = cleanInput tiles
    loopSet = Set.fromList . map snd $ bfs (neighbors mat) start
    -- replace each tile not in the loop with "."
    cleanedTiles = [ [ if (i, j) `Set.member` loopSet then tile else NoPipe
                     | (j, tile) <- zip [0..] row
                     ] 
                   | (i, row) <- zip [0..] tiles'
                   ]
    countRow = sum
            . map length
            . keepOdds
            . splitWhen (`elem` [NS, NE, SE])
            . filter (`notElem` [NW, SW])
            . prefilter
            . filter (/= EW)
    prefilter [] = []
    prefilter (NE :  NW : xs) = prefilter xs
    prefilter (SE : SW : xs) = prefilter xs
    prefilter (x:xs) = x : prefilter xs

solve :: Text -> IO ()
solve = aoc parser part1 part2