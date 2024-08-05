-- https://adventofcode.com/2017/day/21
module Day21 (solve) where
import           AOC.Prelude
import           AOC (aoc)
import           AOC.Parser (Parser, char, choice, eol, sepBy1, sepEndBy1, some, scanf)
import qualified Data.HashMap.Strict as Map
import           AOC.List (count, grouped)
import           AOC.Util (times)

type Grid = [[Char]]
type Rules = HashMap Grid Grid

parser :: Parser Rules
parser = makeRules <$> rule `sepEndBy1` eol where
    rule = [scanf|{grid} => {grid}|]
    grid = some tile `sepBy1` "/"
    tile = choice $ map char ".#"

step :: Rules -> Grid -> Grid
step rules grid = unsplit . map (map (rules Map.!)) $ split grid
    where
    d = if even (length grid) then 2 else 3
    split = map (transpose . map (grouped d)) . grouped d
    unsplit = concatMap (map concat . transpose)

initialGrid :: Grid
initialGrid = [".#.", "..#", "###"]

orientations :: Grid -> [Grid]

orientations grid = [z | y <- [grid, reverse grid], z <- take 4 (iterate' rotate y)]
    where
    rotate = transpose . reverse

makeRules :: [(Grid, Grid)] -> Rules
makeRules pairs = Map.fromList [(x', y) | (x, y) <- pairs, x' <- orientations x]

solveFor :: Int -> Rules -> Int
solveFor n rules = count (=='#') . concat $  times n (step rules) initialGrid 

solve :: Text -> IO ()
solve = aoc parser (solveFor 5) (solveFor 18)
