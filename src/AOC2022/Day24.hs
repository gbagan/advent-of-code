-- https://adventofcode.com/2022/day/1
module AOC2022.Day24 (solve) where
import           RIO hiding (some)
import           Text.Megaparsec (sepEndBy1, some)
import           Text.Megaparsec.Char (char, eol)
import           Linear.V3 (V3(..))
import           Util (Parser, aoc)
import           Util.Matrix (Matrix)
import qualified Util.Matrix as M
import           Util.Search (distance)

data Tile = North | South | West | East | Wall | Empty deriving (Eq)

parser :: Parser (Matrix Tile)
parser = M.fromList <$> some tile `sepEndBy1` eol where
    tile = North <$ char '^' <|> South <$ char 'v' <|> West <$ char '<'
       <|> East <$ char '>' <|> Wall <$ char '#' <|> Empty <$ char '.' 

directions :: [V3 Int]
directions = [V3 1 0 0, V3 1 1 0, V3 1 0 1, V3 1 0 (-1), V3 1 (-1) 0]

travelDuration :: Matrix Tile -> (Int, Int) -> (Int, Int) -> Int -> Maybe Int
travelDuration grid (startr, startc) dest startTime = distance nborFunc destFunc start
    where
    start = V3 startTime startr startc
    nrows = M.nbRows grid
    ncols = M.nbColumns grid
    destFunc (V3 _ r c) = (r, c) == dest 
    nborFunc v = [v' | dir <- directions, let v' = v + dir, isAllowed v']
    isAllowed (V3 t r c) = grid M.!? (r, c) `notElem` [Nothing, Just Wall]
                        && grid M.! ((r - t - 1) `mod` (nrows-2) + 1, c) /= South
                        && grid M.! ((r + t - 1) `mod` (nrows-2) + 1, c) /= North
                        && grid M.! (r, (c + t - 1) `mod` (ncols-2) + 1) /= West
                        && grid M.! (r, (c - t - 1) `mod` (ncols-2) + 1) /= East

part1 :: Matrix Tile -> Maybe Int
part1 grid = travelDuration grid (0, 1) (nrows-1, ncols-2) 0 where
    nrows = M.nbRows grid
    ncols = M.nbColumns grid

part2 :: Matrix Tile -> Maybe Int
part2 grid = do
    let nrows = M.nbRows grid
    let ncols = M.nbColumns grid
    time1 <- travelDuration grid (0, 1) (nrows-1, ncols-2) 0
    time2 <- travelDuration grid (nrows-1, ncols-2) (0, 1) time1
    time3 <- travelDuration grid (0, 1) (nrows-1, ncols-2) (time1+time2)
    pure $ time1 + time2 + time3

solve :: MonadIO m => Text -> m ()
solve = aoc parser part1 part2