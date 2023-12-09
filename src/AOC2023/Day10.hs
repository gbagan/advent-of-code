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
import           AOC.Util (count, listTo2dMap, adjacentPoints)

data Tile = NS | EW | NE | NW | SW | SE | NoPipe | Start deriving (Eq)
type Input = [[Tile]]

parser :: Parser Input
parser =  some tile `sepEndBy1` eol where
    tile = choice [NS <$ "|", EW <$ "-", NE <$"L", NW <$ "J", SW <$ "7", SE <$ "F", NoPipe <$ ".", Start <$ "S"]

loopBfs :: Input -> [(Int, (Int, Int))]
loopBfs tiles = bfs neighbors start where
    start = head [(i, j) | ((j, i), Start) <- Map.toList mat]
    neighbors' (i, j) = case mat Map.!? (j, i) of
        Just NS -> [(i-1, j), (i+1, j)]
        Just EW -> [(i, j-1), (i, j+1)]
        Just NE -> [(i-1, j), (i, j+1)]
        Just NW -> [(i, j-1), (i-1, j)]
        Just SW -> [(i+1, j), (i, j-1)]
        Just SE -> [(i, j+1), (i+1, j)]
        Just Start -> adjacentPoints (i, j)
        _ -> []
    neighbors (i, j) = neighbors' (i, j) & filter \pos -> (i, j) `elem` neighbors' pos
    mat = listTo2dMap tiles

part1 :: Input -> Int
part1 = maximum . map fst . loopBfs

keepOdds :: [a] -> [a]
keepOdds (_:y:xs) = y : keepOdds xs
keepOdds _ = []

part2 :: Input -> Int
part2 tiles = sum . map countRow $ tilesWithIndex where
    countRow = sum . map (count notInLoop) . keepOdds . splitWhen isSeparator
    notInLoop (i, j, _) =  not $ Set.member (j, i) loopSet
    isSeparator (i, j, tile) = (i, j) `Set.member` loopSet && tile `elem` [NS, NW, NE, Start]
    loopSet = Set.fromList . map snd $ loop tiles
    tilesWithIndex = [[(i, j, tile) | (j, tile) <- zip [0..] row] | (i, row) <- zip [0..] tiles] 

solve :: Text -> IO ()
solve = aoc parser part1 part2