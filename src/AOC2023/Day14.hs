-- https://adventofcode.com/2023/day/14
module AOC2023.Day14 (solve) where
import           AOC.Prelude hiding (cycle, empty)
import           Data.List ((!!))
import qualified Data.HashMap.Strict as Map
import           AOC (aoc)
import           AOC.Parser (Parser, sepEndBy1, eol, some)

data Rock = Empty | Round | Cube deriving (Eq, Enum)
type Grid = [[Rock]]

instance Hashable Rock where
    hashWithSalt s rock = s `hashWithSalt` fromEnum rock

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

-- return the first two indices of the same element in a infinite list of elements
findRepetition :: Hashable a => [a] -> (Int, Int)
findRepetition = go Map.empty . zip [0..] where
    go m ((i, x) : xs) =
        case m Map.!? x of
            Just j -> (j, i)
            Nothing -> go (Map.insert x i m) xs
    go _ [] = error "findRepetition: not an infinite list"

-- compute the laod of a grid
load :: Grid -> Int
load grid = sum [len - i | (i, row) <- zip [0..] grid, Round <- row] where
    len = length grid

part1 :: Grid -> Int
part1 = load . transpose . tilt . transpose

part2 :: Grid -> Int
part2 grid = load . transpose $ grids !! z where 
    (x, y) = findRepetition grids
    z = x + (1000000000 - x) `mod` (y-x)
    grids = iterate' cycle (transpose grid)
    step = reverse . transpose . tilt
    cycle = step . step . step . step

solve :: Text -> IO ()
solve = aoc parser part1 part2