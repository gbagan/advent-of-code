-- https://adventofcode.com/2023/day/14
module AOC2023.Day14 (solve) where
import           AOC.Prelude hiding (cycle, empty)
import           Data.List ((!!))
import           AOC (aoc)
import qualified Data.HashMap.Strict as Map
import           AOC.Parser (Parser, sepEndBy1, eol, some)
import           AOC.Util (flattenWithIndex, splitWhen)

data Rock = Empty | Round | Cube deriving (Eq, Enum)
type Grid = [[Rock]]

instance Hashable Rock where
    hashWithSalt s rock = s `hashWithSalt` fromEnum rock

parser :: Parser Grid
parser = some rock `sepEndBy1` eol where
    rock = Empty <$ "." <|> Round <$ "O" <|> Cube <$ "#"

-- tilt to West
tilt :: Grid -> Grid
tilt = map perRow where
    perRow = intercalate [Cube] . map go . splitWhen (==Cube)
    go xs = rounded ++ empty where (rounded, empty) = partition (==Round) xs

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
load grid = sum . map score $ flattenWithIndex grid where
    score (i, _, Round) = len - i
    score _ = 0
    len = length grid

part1 :: Grid -> Int
part1 = load . transpose . tilt . transpose

part2 :: Grid -> Int
part2 grid = load . transpose $ grids !! y' where 
    (x, y) = findRepetition grids
    y' = x + (1000000000 - x) `mod` (y-x)
    grids = iterate' cycle (transpose grid)
    step = reverse . transpose . tilt
    cycle = step . step . step . step

solve :: Text -> IO ()
solve = aoc parser part1 part2