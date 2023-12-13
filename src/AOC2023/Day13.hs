-- https://adventofcode.com/2023/day/13
module AOC2023.Day13 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import qualified Data.Vector as V
import           Data.Vector ((!))
import           AOC.Parser (Parser, sepEndBy1, some, eol)

data Tile = Ash | Rock deriving (Eq)
type Grid = [[Tile]]

parser :: Parser [Grid]
parser = (some tile `sepEndBy1` eol) `sepEndBy1` eol where
    tile = Ash <$ "." <|> Rock <$ "#"
 
-- lazy version of length xs == n
lengthEq :: Int -> [a] -> Bool
lengthEq n [] = n == 0
lengthEq 0 _ = False
lengthEq n (_:xs) = lengthEq (n-1) xs

-- is there a reflection between row x and row x+1 with exactly nbDiff differences
reflect :: Int -> Vector [Tile] -> Int -> Bool
reflect nbDiffs vec x = lengthEq nbDiffs difference where
    difference = filter id $ zipWith (/=) list1 list2
    list1 = concatMap (vec!) range
    list2 = concatMap (\i -> vec ! (2 * x - i - 1)) range
    n = V.length vec
    range = if 2 * x < n then [0..x-1] else [x..n-1]

solveFor :: Int -> [Grid] -> Int
solveFor nbDiffs = sum . map score where
    score grid = (100*) <$> mirror grid <|> mirror (transpose grid) ?: 0  
    mirror grid = find (reflect nbDiffs vgrid) [1..n-1] where
        vgrid = V.fromList grid
        n = V.length vgrid

solve :: Text -> IO ()
solve = aoc parser (solveFor 0) (solveFor 1)