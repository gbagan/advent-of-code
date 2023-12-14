-- https://adventofcode.com/2023/day/14
module AOC2023.Day14 (solve) where
import           AOC.Prelude
import           Data.List ((!!))
import           AOC (aoc)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as Map
import           AOC.Parser (Parser, sepEndBy1, eol, some)
import           AOC.Util (flattenWithIndex)

data Rock = Empty | Round | Cube deriving (Eq, Enum)
data Direction = West | East | North | South
type Grid = [[Rock]]

instance Hashable Rock where
    hashWithSalt s rock = s `hashWithSalt` fromEnum rock

parser :: Parser Grid
parser = some rock `sepEndBy1` eol where
    rock = Empty <$ "." <|> Round <$ "O" <|> Cube <$ "#"

finalPositions :: [Rock] -> [Int]
finalPositions = go 0 . zip [0..] where
    go lastIndex = \case
        [] -> []
        ((i, Cube):xs) -> go (i + 1) xs
        ((_, Round):xs) -> lastIndex : go (lastIndex + 1) xs
        (_:xs) -> go lastIndex xs

tilt :: Grid -> Grid
tilt = map perRow where
    perRow row = V.toList . (V.// map (,Round) positions) . V.map toEmpty $ vec where
        vec = V.fromList row
        positions = finalPositions row
        toEmpty Round = Empty
        toEmpty x = x

tiltToDirection :: Direction -> Grid -> Grid
tiltToDirection direction grid = case direction of
    West -> tilt grid
    East -> map reverse . tilt $ map reverse grid
    North -> transpose . tilt $ transpose grid
    South -> reverse . transpose . tilt . transpose $ reverse grid

findRepetition :: Hashable a => [(Int, a)] -> (Int, Int)
findRepetition = go Map.empty where
    go m ((i, x) : xs) =
        case m Map.!? x of
            Just j -> (j, i)
            Nothing -> go (Map.insert x i m) xs
    go _ [] = error "cannot happen: infinite list"

load :: Grid -> Int
load grid = sum . map score $ flattenWithIndex grid where
    score (i, _, Round) = len - i
    score _ = 0
    len = length grid

part1 :: Grid -> Int
part1 = load . tiltToDirection North

part2 :: Grid -> Int
part2 grid = load . snd $ grids !! y' where 
    (x, y) = findRepetition grids
    y' = x + (1000000000 - x) `mod` (y-x)
    grids = zip [0..] (scanl' (flip tiltToDirection) grid directions)
    directions = cycle [North, West, South, East]

solve :: Text -> IO ()
solve = aoc parser part1 part2