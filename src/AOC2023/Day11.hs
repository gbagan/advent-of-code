-- https://adventofcode.com/2023/day/11
module AOC2023.Day11 (solve) where
import           AOC.Prelude
import qualified Data.Vector as V
import           AOC (aoc)
import           AOC.Parser (Parser, eol, sepEndBy1, some)

type Coord = (Int, Int)
type Grid = [[Bool]]

parser :: Parser Grid
parser = some isGalaxy `sepEndBy1` eol where
    isGalaxy = False <$ "." <|> True <$ "#"

manhattan :: Coord -> Coord -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

gridToList :: Grid -> [Coord]
gridToList grid = [(i, j) | (i, row) <- zip [0..] grid, (j, True) <- zip [0..] row] 

countEmptyRows :: Grid -> Vector Int
countEmptyRows = V.fromList . drop 1 . scanl' (\acc row -> if all not row then acc+1 else acc) 0

solveWith :: Int -> Grid -> Int
solveWith expand grid = sum (manhattan <$> expGalaxies <*> expGalaxies) `div` 2 where
    galaxies = gridToList grid
    emptyRows = countEmptyRows grid
    emptyCols = countEmptyRows (transpose grid)
    expGalaxies = [(x + (expand - 1) * emptyRows V.! x, y + (expand - 1) * emptyCols V.! y) | (x, y) <- galaxies]

solve :: Text -> IO ()
solve = aoc parser (solveWith 2) (solveWith 1000000)