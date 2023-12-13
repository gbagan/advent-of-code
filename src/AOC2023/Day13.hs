-- https://adventofcode.com/2023/day/13
module AOC2023.Day13 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import qualified Data.Vector as V
import           Data.Vector ((!))
import           AOC.Parser (Parser, sepEndBy1, some, eol)
import           AOC.Util (count)

data Tile = Ash | Rock deriving (Eq)
type Grid = [[Tile]]

parser :: Parser [Grid]
parser = (some tile `sepEndBy1` eol) `sepEndBy1` eol where
    tile = Ash <$ "." <|> Rock <$ "#"

difference :: [Tile] -> [Tile] -> Int
difference l1 l2 = count id $ zipWith (/=) l1 l2

isSymetry :: Int -> Vector [Tile] -> Int -> Bool
isSymetry diff v x = total == diff where
    total = sum $ range <&> \i -> difference (v ! i) (v ! (2 * x - i - 1))
    n = V.length v
    range = if 2 * x < n then [0..x-1] else [x..n-1]

solveFor :: Int -> [Grid] -> Int
solveFor diff = sum . map score where
    score grid = 100 * symetries grid + symetries (transpose grid)
    symetries grid = sum $ filter (isSymetry diff vgrid) [1..n-1] where
        vgrid = V.fromList grid
        n = V.length vgrid

solve :: Text -> IO ()
solve = aoc parser (solveFor 0) (solveFor 1)