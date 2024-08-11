-- https://adventofcode.com/2018/day/18
module Day18 (solve) where
import           AOC.Prelude hiding (get, toList)
import           AOC (aoc)
import           Data.Massiv.Array hiding (map)
import           Data.Massiv.Array.Unsafe (makeUnsafeStencil)
import           AOC.List (count)
import           AOC.Parser (Parser, char, sepEndBy1, some, eol)
import           AOC.Util (times, manyTimes)

newtype Grid = Grid (Matrix U Char) deriving (Eq)
instance Hashable Grid where
    hashWithSalt salt (Grid grid) = hashWithSalt salt (toList grid)

directions :: [Ix2]
directions = [ 1 :. 1
             , 1 :. 0
             , 1 :. (-1)
             , 0 :. (-1)
             , (-1) :. (-1)
             , (-1) :. 0
             , (-1) :. 1
             , 0 :. 1
            ]

parser :: Parser Grid
parser = Grid . fromLists' Seq <$> some tile `sepEndBy1` eol where
    tile = char '#' <|> char '|' <|> char '.'

stencil :: Stencil Ix2 Char Char
stencil = makeUnsafeStencil (Sz2 3 3) (0 :. 0) \_ get ->
    let trees = count (=='|') (map get directions)
        lumberyards = count (=='#') (map get directions)
    in case get (0 :. 0) of
        '.' | trees >= 3                     -> '|'
        '|' | lumberyards >= 3               -> '#' 
        '#' | trees == 0 || lumberyards == 0 -> '.'
        x -> x

step :: Grid -> Grid
step (Grid grid) = Grid . compute . dropWindow $ mapStencil (Fill ' ') stencil grid

score :: Grid -> Int
score (Grid grid) = trees * lumberyards where
    list = toList grid
    trees = count (=='|') list
    lumberyards = count (=='#') list

part1 :: Grid -> Int
part1 = score . times 10 step

part2 :: Grid -> Int
part2 = score . manyTimes 1_000_000_000 step

solve :: Text -> IO ()
solve = aoc parser part1 part2
