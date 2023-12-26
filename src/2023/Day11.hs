-- https://adventofcode.com/2023/day/11
module Day11 (solve) where
import           AOC.Prelude
import qualified Data.Vector as V
import           AOC (aoc)
import           AOC.V2 (V2(..), manhattan)
import           AOC.Parser (Parser, eol, sepEndBy1, some)

type Grid = [[Bool]]

parser :: Parser Grid
parser = some isGalaxy `sepEndBy1` eol where
    isGalaxy = False <$ "." <|> True <$ "#"

countEmptyRows :: Grid -> Vector Int
countEmptyRows = V.fromList . drop 1 . scanl' (\acc row -> if all not row then acc+1 else acc) 0

solveFor :: Int -> Grid -> Int
solveFor expand grid = sum (manhattan <$> expGalaxies <*> expGalaxies) `div` 2 where
    galaxies = [ V2 i j | (i, row) <- zip [0..] grid, (j, True) <- zip [0..] row]
    emptyRows = countEmptyRows grid
    emptyCols = countEmptyRows (transpose grid)
    expGalaxies = [ V2 (x + (expand - 1) * emptyRows V.! x)
                       (y + (expand - 1) * emptyCols V.! y)
                  | V2 x y <- galaxies]

solve :: Text -> IO ()
solve = aoc parser (solveFor 2) (solveFor 1000000)