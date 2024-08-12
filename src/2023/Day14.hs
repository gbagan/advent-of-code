-- https://adventofcode.com/2023/day/14
module Day14 (solve) where
import           AOC.Prelude hiding (cycle, empty)
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, eol, some)
import           AOC.Util (manyTimes)

data Rock = Empty | Round | Cube deriving (Eq, Enum, Generic)
type Grid = [[Rock]]

instance Hashable Rock

parser :: Parser Grid
parser = some rock `sepEndBy1` eol where
    rock = Empty <$ "." <|> Round <$ "O" <|> Cube <$ "#"

-- tilt in West direction
tilt :: Grid -> Grid
tilt = map (go 0) where
    go n = \case
        (Empty:xs) -> go (n+1) xs
        (Round:xs) -> Round : go n xs
        (Cube:xs) -> replicate n Empty ++ Cube : go 0 xs
        _        -> replicate n Empty

-- compute the laod of a grid
load :: Grid -> Int
load grid = sum [len - i | (i, row) <- zip [0..] grid, Round <- row] where
    len = length grid

part1 :: Grid -> Int
part1 = load . transpose . tilt . transpose


part2 :: Grid -> Int
part2 grid = load . transpose $ manyTimes 1_000_000_000 cycle (transpose grid) where
    step = reverse . transpose . tilt
    cycle = step . step . step . step

solve :: Text -> IO ()
solve = aoc parser part1 part2