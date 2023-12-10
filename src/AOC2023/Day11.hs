-- https://adventofcode.com/2023/day/11
module AOC2023.Day11 (solve) where
import           AOC.Prelude
import qualified Data.Vector as V
import           AOC (aoc)
import           AOC.Parser (Parser, eol, sepEndBy1, some)

parser :: Parser [[Bool]]
parser = some isGalaxy `sepEndBy1` eol where
    isGalaxy = False <$ "." <|> True <$ "#"


gridToList :: [[Bool]] -> [(Int, Int)]
gridToList grid = [(i, j) | (i, row) <- zip [0..] grid, (j, isGalaxy) <- zip [0..] row, isGalaxy] 

emptyLines :: [[Bool]] -> Vector Int
emptyLines = V.fromList . drop 1 . scanl' (\acc row -> if all not row then acc+1 else acc) 0

solveWith :: Int -> [[Bool]] -> Int
solveWith expand grid = sum (manhattan <$> galaxies <*> galaxies) `div` 2 where
    galaxies = gridToList grid
    emptyRows = emptyLines grid
    emptyCols = emptyLines (transpose grid)
    manhattan (x1, y1) (x2, y2) = dist + expandX + expandY where
        dist = abs (x1 - x2) + abs (y1 - y2)
        expandX = (expand - 1) * abs (emptyRows V.! x1 - emptyRows V.! x2)
        expandY = (expand - 1) * abs (emptyCols V.! y1 - emptyCols V.! y2)

solve :: Text -> IO ()
solve = aoc parser (solveWith 2) (solveWith 1000000)
